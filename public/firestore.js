
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
        else if (object.__doc__) return reference(object.path)
        else {
            let newobj = {}
            for (const [key, value] of Object.entries(object)) {
                newobj[key] = encode(value)
            }
            return newobj
        }
    }

    function decode(object) {
        if (typeof object != 'object' || object === null) return object
        if (object instanceof Array) {
            let array = []
            for (const value of object) {
                array.push(decode(value))
            }
            return array
        }
        else if (object instanceof firebase.firestore.DocumentReference) {
            return getData(object.path.split('/')) || {
                data: null,
                path: object.path.split('/'),
                status: 'loading',
                __doc__: true,
            }
        }
        else {
            let newobj = {}
            for (const [key, value] of Object.entries(object)) {
                newobj[key] = decode(value)
            }
            return newobj
        }
    }

    function update(object) {
        if (typeof object != 'object' || object === null) return object
        if (object instanceof Array) {
            let array = []
            for (const value of object) {
                array.push(update(value))
            }
            return array
        }
        else if (object.__doc__) {
            if (object.updated) return object
            object.updated = true
            let newobj = getData(object.path)
            if (!newobj) return object
            newobj.updated = true
            newobj.__doc__ = true
            newobj.data = update(newobj.data)
            return newobj
        }
        else {
            let newobj = {}
            for (const [key, value] of Object.entries(object)) {
                newobj[key] = update(value)
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

    let data;

    function getData(path) {
        let d = data;
        let i = 0;
        while (true) {
            if (i >= path.length) return d
            d = d[path[i]]
            if (!d) return null
            i++
            if (i >= path.length) return d
            d = d.documents[path[i]]
            if (!d) return null
            i++
        }
    }
    let c = 0;

    function request(path){
        let d = getData(path)
        if (d && d.status != 'loading') return

        if (c++ > 100) return


        if (d && d.asked) return
        if (d) d.asked = true
        else data[path[0]].documents[path[1]] = {asked: true}

        console.log("req", path, d)

        reference(path).get().then(doc => {
            const document = {
                data: decode(doc.data()),
                path: path,
                status: doc.exists ? 'uptodate' : 'failure'
            }

            data[path[0]].documents[path[1]] = document

            data = update(data)

            console.log("data", path, document, data)
            if (app.ports.watcherPort) {
                app.ports.watcherPort.send(data)

                // setTimeout(() => {app.ports.watcherPort.send(data)}, 2000)
            }
        }).catch(err => {
            console.log("fetch error", err)
        })
    }

    if (app.ports.updatePort) {
        app.ports.updatePort.subscribe(v => {
            console.log("update", v);
            data = v.value
            for (const upd of v.documents) {
                switch (upd.type) {
                case "whole":
                    const encdata = encode(upd.value.data)
                    console.log('enc', encdata)
                    reference(upd.path).set(encdata)
                    break
                case "delete":
                    reference(upd.path).delete()
                    break
                }
            }
            for (const path of v.requests) request(path)
        })
    }
}
