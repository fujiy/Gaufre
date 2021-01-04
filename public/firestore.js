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

    let data;

    request = (path, reference) => {
        console.log("req", path, reference)
        db.collection(path[0]).doc(path[1]).get().then(doc => {
            console.log(doc)
            let document
            if (doc.exists) {
                document = doc.data()
                document.path = path
                document.status = 'uptodate'
            }
            else document = { path: path, status: 'failure' }

            data[path[0]].documents[path[1]] = document

            console.log("data", data)
            if (app.ports.watcherPort) {
                app.ports.watcherPort.send(data)
            }
        })
    }

    if (app.ports.updatePort) {
        app.ports.updatePort.subscribe(v => {
            data = v.value
            for (const upd of v.documents) {
                switch (upd.type) {
                case "whole":
                    delete upd.value.path
                    delete upd.value.status
                    db.collection(upd.path[0]).doc(upd.path[1])
                        .set(upd.value)
                    break
                case "delete":
                    db.collection(upd.path[0]).doc(upd.path[1]).delete()
                    break
                }
            }
            console.log("update", v);
        })
    }
}
