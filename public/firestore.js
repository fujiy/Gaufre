let request;

class DataRequester extends HTMLElement {
    connectedCallback() {
        request(this.path, this.reference);
        this.hidden = true;
    }
}
customElements.define('data-requester', DataRequester);


function initialize(app) {
    const db = firebase.firestore()


    function encode(object) {
        console.log(object)
        if (typeof object != 'object') return object
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

    function reference(path) {
        let ref = db;
        let i = 0;
        while (true) {
            if (i >= path.length) break;
            ref = ref.collection(path[i])
            i++
            if (i >= path.length) break;
            ref = ref.doc(path[i])
            i++
        }
        return ref;
    }


    let data;

    request = (path, ref) => {
        console.log("req", path, ref)
        reference(path).get().then(doc => {
            const document = {
                data: doc.data(),
                path: path,
                status: doc.exists ? 'uptodate' : 'failure'
            }

            data[path[0]].documents[path[1]] = document

            console.log("data", data)
            if (app.ports.watcherPort) {
                app.ports.watcherPort.send(data)
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
            for (const path of v.requests) request(path, null)
        })
    }
}
