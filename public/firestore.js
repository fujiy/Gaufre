
function initialize(app) {
    const db = firebase.firestore()

    function encode(object) {
        if (typeof object != 'object' || object === null) return object
        if (object instanceof Array) {
            let array = []
            for (const value of object) {
                array.push(encode(value))
            }
            return array
        }
        else if (object.__path__) return reference(object.__path__)
        else {
            let newobj = {}
            for (const [key, value] of Object.entries(object)) {
                newobj[key] = encode(value)
            }
            return newobj
        }
    }

    function makeDoc(doc) {
        return {status: doc.data() ? "uptodate" : "failure",
                value: doc.data() || null}
    }

    function reference(path) {
        let ref = db;
        let i = 0;
        while (true) {
            if (i >= path.length) break
            ref = ref.collection(path[i])
            i++
            if (i >= path.length) break
            ref = ref.doc(path[i])
            i++
        }
        return ref
    }

    function makePathMap(path, item) {
        const object = {}
        let obj = object
        let i = 0
        while (i < path.length) {
            obj.item = null
            obj.sub = {}
            obj.sub[path[i]] = {}
            obj = obj.sub[path[i]]
            i++;
        }
        obj.item = item
        obj.sub = {}
        return object
    }

    function makePathMapAt(path, sub) {
        const object = {}
        let obj = object
        let i = 0
        while (i < path.length) {
            obj.item = null
            obj.sub = {}
            obj.sub[path[i]] = {}
            obj = obj.sub[path[i]]
            i++;
        }
        obj.item = sub.item
        obj.sub = sub.sub
        return object
    }

    function listen(path, paths, tree) {
        if (paths.item && !tree.__listener__) {
            if (path.length % 2 == 1) {
                tree.__listener__ =
                    reference(path).onSnapshot({
                        includeMetadataChanges: true
                    }, querySnapshot => {
                        const map = {item: null, sub: {}}
                        querySnapshot.forEach( doc => {
                            map.sub[doc.id] = {item: makeDoc(doc), sub: {}}
                        })
                        const updates = makePathMapAt(path, map)

                        console.log("snapshots", showPathMap(updates))
                        if (!querySnapshot.metadata.hasPendingWrites)
                            sendSub({updates: updates})
                    })
            }
            else {
                tree.__listener__ =
                    reference(path).onSnapshot({
                        includeMetadataChanges: true
                    }, doc => {
                        const updates = makePathMap(path, makeDoc(doc))

                        console.log("snapshot", doc, showPathMap(updates)[0]);
                        if (!doc.metadata.hasPendingWrites)
                            sendSub({updates: updates})
                    })
            }
        }

        for (const [id, subPaths] of Object.entries(paths.sub)) {
            if (!tree[id]) tree[id] = {}
            const subPath = path.slice()
            subPath.push(id)
            listen(subPath, subPaths, tree[id])
        }
    }

    function unlisten(paths, tree) {
        if (!tree) return
        if (paths.item && tree.__listener__) {
            tree.__listener__()
            tree.__listener__ = null
        }
        for (const [id, subPaths] of Object.entries(paths.sub)) {
            unlisten(subPaths, tree[id])
        }
    }

    function update(path, pathMap) {
        if (pathMap.item) {
            const upd = pathMap.item
            console.log("update", path, upd)
            switch (upd.type) {
            case "set":
                reference(path).set(encode(upd.value.value))
                break
            }
        }
        for (const [id, subMap] of Object.entries(pathMap.sub)) {
            const subPath = path.slice()
            subPath.push(id)
            update(subPath, subMap)
        }
    }
    function showPathMap(pm) {
        function go(path, pathMap, result) {
            if (pathMap.item) {
                const r = path.slice()
                r.push(pathMap.item)
                result.push(r)
            }
            for (const [id, subMap] of Object.entries(pathMap.sub)) {
                const subPath = path.slice()
                subPath.push(id)
                go(subPath, subMap, result)
            }
            return result
        }
        return go([], pm, []);
    }

    function sendSub(v) {
        if (app.ports.firestoreSubPort)
            app.ports.firestoreSubPort.send(v)
    }

    let listeners = {}

    window.listeners = listeners

    if (app.ports.firestoreCmdPort) {
        app.ports.firestoreCmdPort.subscribe(v => {
            console.log("command",
                        showPathMap(v.listen),
                        showPathMap(v.unlisten),
                        showPathMap(v.updates)
                       );

            listen([], v.listen, listeners)
            unlisten(v.unlisten, listeners)
            update([], v.updates)
        })
    }
}
