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
    } else if (object.__path__) return reference(object.__path__);
    else {
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
            ref.where(colPath.field, colPath.op, colPath.value)
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

  // function reference(path) {
  //   let ref = db;
  //   let i = 0;
  //   while (true) {
  //     if (i >= path.length) break;
  //     ref = ref.collection(path[i]);
  //     i++;
  //     if (i >= path.length) break;
  //     ref = ref.doc(path[i]);
  //     i++;
  //   }
  //   return ref;
  // }

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
    obj.q = [];
    obj.sub = sub.sub;
    return object.sub;
  }

  function queryKey(item) {
    return item.field + item.op + Json.stringify(item.value);
  }

  function listen(rootMap) {
    function goCol(colMap, tree, ref) {
      if (colMap.item && !tree._listener) {
        tree._listener = ref.onSnapshot(
          {
            includeMetadataChanges: true,
          },
          (querySnapshot) => {
            const map = { item: null, sub: [] };
            querySnapshot.forEach((doc) => {
              map.sub.push({ item: makeDoc(doc), sub: [], id: doc.id });
            });
            const updates = makePathMapAt(ref.path, map);

            console.log("snapshots", showPathMap(updates));

            if (!querySnapshot.metadata.hasPendingWrites)
              sendSub({ updates: updates });
          }
        );
      }

      for (const doc of colMap.sub) {
        if (!tree[doc.id]) tree[doc.id] = {};
        goDoc(doc, tree[doc.id], ref.doc(doc.id));
      }
      for (const col of colMap.q) {
        if (!tree[queryKey(col)]) tree[queryKey(col)] = {};
        goCol(
          col,
          tree[queryKey(col)],
          ref.where(col.field, col.op, col.value)
        );
      }
    }

    function goDoc(docMap, tree, ref) {
      if (docMap.item && !tree._listener) {
        tree._listener = ref.onSnapshot(
          { includeMetadataChanges: true },
          (doc) => {
            const updates = makePathMap(ref.path, makeDoc(doc));

            console.log("snapshot", updates, showPathMap(updates)[0]);
            if (!doc.metadata.hasPendingWrites) sendSub({ updates: updates });
          }
        );
      }
      for (const col of docMap.sub) {
        if (!tree[col.id]) tree[col.id] = {};
        goCol(col, tree[col.id], ref.collection(col.id));
      }
    }

    for (const col of rootMap) {
      if (!listeners[col.id]) listeners[col.id] = {};
      goCol(col, listeners[col.id], db.collection(col.id));
    }
  }

  // function listen(path, paths, tree) {
  //   if (path.length > 0 && paths.item && !tree._listener) {
  //     if (path.length % 2 == 1) {
  //       tree._listener = reference(path).onSnapshot(
  //         {
  //           includeMetadataChanges: true,
  //         },
  //         (querySnapshot) => {
  //           const map = { item: null, sub: {} };
  //           querySnapshot.forEach((doc) => {
  //             map.sub[doc.id] = { item: makeDoc(doc), sub: {} };
  //           });
  //           const updates = makePathMapAt(path, map);

  //           console.log(
  //             "snapshots",
  //             querySnapshot.metadata,
  //             showPathMap(updates)
  //           );
  //           if (!querySnapshot.metadata.hasPendingWrites)
  //             sendSub({ updates: updates });
  //         }
  //       );
  //     } else {
  //       tree._listener = reference(path).onSnapshot(
  //         {
  //           includeMetadataChanges: true,
  //         },
  //         (doc) => {
  //           const updates = makePathMap(path, makeDoc(doc));

  //           console.log("snapshot", doc.metadata, showPathMap(updates)[0]);
  //           if (!doc.metadata.hasPendingWrites) sendSub({ updates: updates });
  //         }
  //       );
  //     }
  //   }

  //   for (const [id, subPaths] of Object.entries(paths.sub)) {
  //     if (!tree[id]) tree[id] = {};
  //     const subPath = path.slice();
  //     subPath.push(id);
  //     listen(subPath, subPaths, tree[id]);
  //   }
  // }

  function unlisten(rootMap) {
    function goCol(colMap, tree) {
      if (!tree) return;
      if (colMap.item && tree._listener) {
        tree._listener();
        tree._listener = null;
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
        tree._listener();
        tree._listener = null;
      }

      for (const col of docMap.sub) {
        goCol(col, tree[col.id]);
      }
    }

    for (const col of rootMap) {
      goCol(col, listeners[col.id]);
    }
  }

  // function unlisten(paths, tree) {
  //   if (!tree) return;
  //   if (paths.item && tree._listener) {
  //     tree._listener();
  //     tree._listener = null;
  //   }
  //   for (const [id, subPaths] of Object.entries(paths.sub)) {
  //     unlisten(subPaths, tree[id]);
  //   }
  // }

  function update(rootMap) {
    function goCol(colMap, ref) {
      if (colMap.item) {
        switch (colMap.item.type) {
          case "add":
            ref.set(encode(colMap.item.value.value));
            break;
        }
      }

      for (const doc of colMap.sub) {
        goDoc(doc, ref.doc(doc.id));
      }
      for (const col of colMap.q) {
        goCol(col, ref.where(col.field, col.op, col.value));
      }
    }

    function goDoc(docMap, ref) {
      if (docMap.item) {
        switch (docMap.item.type) {
          case "set":
            ref.set(encode(docMap.item.value.value));
            break;
          case "delete":
            ref.set(encode(docMap.item.value.value));
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

  // function update(path, pathMap) {
  //   if (pathMap.item) {
  //     const upd = pathMap.item;
  //     console.log("update", path, upd);
  //     switch (upd.type) {
  //       case "set":
  //         reference(path).set(encode(upd.value.value));
  //         break;
  //     }
  //   }
  //   for (const [id, subMap] of Object.entries(pathMap.sub)) {
  //     const subPath = path.slice();
  //     subPath.push(id);
  //     update(subPath, subMap);
  //   }
  // }

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
        subPath.push([col.field, col.op, col.value]);
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

  // function showPathMap(pm) {
  //   function go(path, pathMap, result) {
  //     if (pathMap.item) {
  //       const r = path.slice();
  //       r.push(pathMap.item);
  //       result.push(r);
  //     }
  //     for (const subMap of pathMap.sub) {
  //       const subPath = path.slice();
  //       subPath.push(subMap.id);
  //       go(subPath, subMap, result);
  //     }
  //     return result;
  //   }
  //   return go([], { sub: pm }, []);
  // }

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
