
# NkADMIN

## API

### Introduction

Once the user is authenticated, it must first create an _admin session_, connected to an specific _domain_, using the `create` command. The server will send the information to draw:

* _Frame_: Information not related to any specific domain like user name, icon, etc.
* _Tree_: Information to build the _menu tree_ for this domain.
* _Detail_: Information to build the _detail pane_ for this domain

If the user selects another domain, the `switch_domain` command can be used, and a new _tree_ and _detail_ block will be sent from the server.

If the user selects an specific object (or entry in the tree), the command `switch_object` can be used, and a new _detail_ block will be returned.

All commands must use `class: "admin"` and no `subclass`.


### Commands

#### create_session

You must supply the `domain`, and the server will return frame, tree and detail (or an error).
You can also subscribe to events related to this object. See [events](#events) for currently supported events.

Example:

```javascript
{
    class: "admin",
    cmd: "create",
    data: {
        domain: "/",
        session_events: ["updated", "destroyed"]
    },
    tid: 1
}

-->

{
    result: "ok",
    data: {
        admin_session_id: "59c03l30ddlekvd93",
        frame: { ... },
        tree: { ... },
        detail: { ... }
    },
    tid: 1
}
```


#### switch_domain

You must supply the `admin_session_id` and `domain` parameters, and the server will return the new _tree_ and _detail_.


#### switch_object

You must supply the `admin_session_id` and `obj_id` parameters, and the server will return the new _detail_.


#### destroy

If you want to stop the admin session (and free resources on the server), you can call this command, using `admin_session_id` parameter (or directly close the connection).


### Events

The admin session supports the following events:

#### created
Sent when the session is first created.


#### updated
Sent when some of the fields currently present at the client have been updated and must be redrawn (to be done)


#### destroyed
Sent if the admin session is destroyed.

