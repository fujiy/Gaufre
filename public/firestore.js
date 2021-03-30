function initialize(app) {
  const db = firebase.firestore();

  function encode(object) {
    if (typeof object != "object" || object === null) return object;
    if (object instanceof Array) {
      let array = [];
      for (const value of object) {
        array.push(encode(value));
      }
      return array;
    } else if (object.__path) return reference(object.__path);
    else if (object.__timestamp == "server") {
      return firebase.firestore.FieldValue.serverTimestamp();
    } else if (object.__timestamp) {
      return new firebase.firestore.Timestamp(
        object.seconds,
        object.nanoseconds
      );
    } else {
      let newobj = {};
      for (const [key, value] of Object.entries(object)) {
        newobj[key] = encode(value);
      }
      return newobj;
    }
  }

  function makeDoc(doc) {
    return {
      id: doc.id,
      status: doc.data() ? "uptodate" : "failure",
      value: doc.data() || null,
    };
  }

  function makeRemovedDoc(doc) {
    return {
      id: doc.id,
      status: "failure",
      value: null,
    };
  }

  function reference(rootPath) {
    function goCol(colPath, ref) {
      switch (colPath.type) {
        case "end":
          return ref;
        case "doc":
          return goDoc(colPath.sub, ref.doc(colPath.id));
        case "query":
          return goCol(
            colPath.sub,
            ref.where(colPath.field, colPath.op, encode(colPath.value))
          );
        default:
          return null;
      }
    }
    function goDoc(docPath, ref) {
      switch (docPath.type) {
        case "end":
          return ref;
        case "col":
          return goCol(docPath.sub, ref.collection(docPath.id));
        default:
          return null;
      }
    }

    switch (rootPath.type) {
      case "root":
        return ref;
      case "col":
        return goCol(rootPath.sub, db.collection(rootPath.id));
      default:
        return null;
    }
  }

  function makePathMap(path, item) {
    path = path.split("/");
    const object = {};
    let obj = object;
    let i = 0;
    while (i < path.length) {
      obj.item = null;
      obj.id = path[i - 1];
      obj.sub = [{}];
      obj.q = [];
      obj = obj.sub[0];
      i++;
    }
    obj.item = item;
    obj.id = path[i - 1];
    obj.q = [];
    obj.sub = [];
    return object.sub;
  }

  function makePathMapAt(path, sub) {
    path = path.split("/");
    const object = {};
    let obj = object;
    let i = 0;
    while (i < path.length) {
      obj.item = null;
      obj.id = path[i - 1];
      obj.sub = [{}];
      obj.q = [];
      obj = obj.sub[0];
      i++;
    }
    obj.item = sub.item;
    obj.id = path[i - 1];
    obj.q = sub.q || [];
    obj.sub = sub.sub;
    return object.sub;
  }

  function queryKey(item) {
    return item.field + item.op + JSON.stringify(item.value);
  }

  function listen(rootMap) {
    function goCol(colMap, tree, ref, builder) {
      if (colMap.item && !tree._listener) {
        tree._listener = ref.onSnapshot(
          {
            includeMetadataChanges: true,
          },
          (querySnapshot) => {
            if (querySnapshot.metadata.hasPendingWrites) return;

            const map = { item: [], sub: [], q: [] };
            console.log(querySnapshot);

            querySnapshot.docs.forEach((doc) => {
              map.item.push(makeDoc(doc));
            });

            const updates = builder(map);
            console.log("snapshots", showPathMap(updates));

            sendSub({ updates: updates });

            tree._startingUp = false;
            if (tree._unlisten && !querySnapshot.metadata.fromCache) {
              tree._listener();
              tree._listener = null;
            }
          }
        );

        tree._startingUp = true;
        tree._unlisten = false;
      }

      for (const doc of colMap.sub) {
        if (!tree[doc.id]) tree[doc.id] = {};
        goDoc(doc, tree[doc.id], ref.doc(doc.id), (m) => {
          m.id = doc.id;
          return builder({ item: null, sub: [m], q: [] });
        });
      }
      for (const col of colMap.q) {
        if (!tree[queryKey(col)]) tree[queryKey(col)] = {};
        goCol(
          col,
          tree[queryKey(col)],
          ref.where(col.field, col.op, encode(col.value)),
          (m) => {
            m.field = col.field;
            m.op = col.op;
            m.value = col.value;
            return builder({ item: null, sub: [], q: [m] });
          }
        );
      }
    }

    function goDoc(docMap, tree, ref, builder) {
      if (docMap.item && !tree._listener) {
        tree._listener = ref.onSnapshot(
          { includeMetadataChanges: true },
          (doc) => {
            if (doc.metadata.hasPendingWrites) return;

            const updates = builder({ item: makeDoc(doc), sub: [] });

            console.log("snapshot", showPathMap(updates)[0]);

            sendSub({ updates: updates });

            tree._startingUp = false;
            if (tree._unlisten && !doc.metadata.fromCache) {
              tree._listener();
              tree._listener = null;
            }
          }
        );
        tree._startingUp = true;
        tree._unlisten = false;
      }
      for (const col of docMap.sub) {
        if (!tree[col.id]) tree[col.id] = {};
        goCol(col, tree[col.id], ref.collection(col.id), (m) => {
          m.id = col.id;
          return builder({ item: null, sub: [m] });
        });
      }
    }

    for (const col of rootMap) {
      if (!listeners[col.id]) listeners[col.id] = {};
      goCol(col, listeners[col.id], db.collection(col.id), (m) => {
        m.id = col.id;
        return [m];
      });
    }
  }

  function unlisten(rootMap) {
    function goCol(colMap, tree) {
      if (!tree) return;
      if (colMap.item && tree._listener) {
        if (tree._startingUp) {
          tree._unlisten = true;
        } else {
          tree._listener();
          tree._listener = null;
        }
      }

      for (const doc of colMap.sub) {
        goDoc(doc, tree[doc.id]);
      }
      for (const col of colMap.q) {
        goCol(col, tree[queryKey(col)]);
      }
    }
    function goDoc(docMap, tree) {
      if (!tree) return;
      if (docMap.item && tree._listener) {
        if (tree._startingUp) {
          tree._unlisten = true;
        } else {
          tree._listener();
          tree._listener = null;
        }
      }

      for (const col of docMap.sub) {
        goCol(col, tree[col.id]);
      }
    }

    for (const col of rootMap) {
      goCol(col, listeners[col.id]);
    }
  }

  function update(rootMap) {
    function goCol(colMap, ref) {
      if (colMap.item) {
        switch (colMap.item.type) {
          case "set":
            ref.add(encode(colMap.item.value.value));
            break;
        }
      }

      for (const doc of colMap.sub) {
        goDoc(doc, ref.doc(doc.id));
      }
      for (const col of colMap.q) {
        goCol(col, ref.where(col.field, col.op, encode(col.value)));
      }
    }

    function goDoc(docMap, ref) {
      if (docMap.item) {
        switch (docMap.item.type) {
          case "set":
            ref.set(encode(docMap.item.value.value));
            break;
          case "delete":
            ref.delete();
            break;
        }
      }
      for (const col of docMap.sub) {
        goCol(col, ref.collection(col.id));
      }
    }

    for (const col of rootMap) {
      goCol(col, db.collection(col.id));
    }
  }

  function showPathMap(rootMap) {
    function goCol(colMap, path, result) {
      if (colMap.item) {
        const r = path.slice();
        r.push(colMap.item);
        result.push(r);
      }

      for (const doc of colMap.sub) {
        const subPath = path.slice();
        subPath.push(doc.id);
        goDoc(doc, subPath, result);
      }
      for (const col of colMap.q) {
        const subPath = path.slice();
        subPath.push(
          col.field + " " + col.op + " " + JSON.stringify(col.value)
        );
        goCol(col, subPath, result);
      }
      return result;
    }

    function goDoc(docMap, path, result) {
      if (docMap.item) {
        const r = path.slice();
        r.push(docMap.item);
        result.push(r);
      }

      for (const col of docMap.sub) {
        const subPath = path.slice();
        subPath.push(col.id);
        goCol(col, subPath, result);
      }
      return result;
    }
    rootResult = [];

    for (const col of rootMap) {
      const subPath = [];
      subPath.push(col.id);
      goCol(col, subPath, rootResult);
    }
    return rootResult;
  }

  function sendSub(v) {
    // console.log("sub", v);
    if (app.ports.firestoreSubPort) app.ports.firestoreSubPort.send(v);
  }

  let listeners = {};

  window.listeners = listeners;

  if (app.ports.firestoreCmdPort) {
    app.ports.firestoreCmdPort.subscribe((v) => {
      console.log(
        "command",
        showPathMap(v.listen),
        showPathMap(v.unlisten),
        showPathMap(v.updates)
      );

      listen(v.listen);
      unlisten(v.unlisten);
      update(v.updates);
    });
  }
}
