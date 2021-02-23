
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

    function listen(path, paths, tree) {
        if (paths.item && !tree.__listener__) {
            tree.__listener__ =
                reference(path).onSnapshot({
                    includeMetadataChanges: true
                    }, doc => {
                    const updates = makePathMap(
                        path,
                        {status: doc.data() ? "uptodate" : "failure",
                         value: doc.data() || null}
                    )

                        console.log("snapshot", path, doc, doc.metadata,
                                    updates);
                    if (!doc.metadata.hasPendingWrites &&
                        app.ports.firestoreSubPort)
                        app.ports.firestoreSubPort.send({updates: updates})
                })
        }

        for (const [id, subPaths] of Object.entries(paths.sub)) {
            if (!tree[id]) tree[id] = {}
            const subPath = path.slice()
            subPath.push(id)
            listen(subPath, subPaths, tree[id])
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

    let listeners = {}

    if (app.ports.firestoreCmdPort) {
        app.ports.firestoreCmdPort.subscribe(v => {
            console.log("command", v);

            listen([], v.listen, listeners)
            update([], v.updates)
        })
    }
}
