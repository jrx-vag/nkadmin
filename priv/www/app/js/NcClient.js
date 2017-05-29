(function(){
	
	// Websocket stuff
	
	ncClient = (function(){
		var ws = null;
		var host = null;
		var port = null;
		var currentTid = 1;
		var outgoingMessages = {};
		var connected = false;

		function wsConnect(_host, _port, _path, _secure) {
			host = _host;
			port = _port;
			path = _path;

			var prot = null;
			if (_secure === undefined || _secure) {
				prot = 'wss://';
			} else {
				prot = 'ws://';
			}
			var webSocketUri = prot + host + ':' + port + '/' + path;
			this.webSocketConnStartDate	 = new Date();
			ws = new WebSocket(webSocketUri);

			// Handlers
			ws.onopen = onWsOpen;
			ws.onclose = onWsClose;
			ws.onmessage = onWsMessage;
			ws.onerror = onWsError;
		}
		
		/************************************/
		/**            GETTERS             **/
		/************************************/
		function getHost() {
			return host;
		}

		function getPort() {
			return port;
		}

		/***********************************/
		/**            EVENTS           **/
		/***********************************/
	
		function dispatchEvent(event, data) {
			// Riot event dispatching
			// This will call the trigger function with a variable number of arguments (in case there are multiple data elements)
			//guf.websocket.trigger.apply(this, arguments);

			// DOM event dispatching
			/* */
			var args;
			if (arguments.length <= 1) {
				args = {};
			} else {
				args = {
					detail: Array.prototype.slice.call(arguments, 1)
				}
			}
			var ncEvent = new CustomEvent(event, args);
			document.dispatchEvent(ncEvent);
			/* */
		}

		/***********************************/
		/**            HANDLERS           **/
		/***********************************/

		function wsClose() {
			log('Forcing websocket close!');
			cancelTimeoutsExcept();
			ws.close();
		}

		function onWsOpen(event) {
			log('onWsOpen', event);
			connected = true;
			dispatchEvent('onWsOpen', event);
		}

		function onWsClose(event) {
			connected = false;
			log('onWsClose', event);
			dispatchEvent('onWsClose', event);
		}

		function onWsMessage(event) {
			debug('onWsMessage', event);
			dispatchEvent('onWsMessage', event.data);
			var json = JSON.parse(event.data);
			if (json['ack']) {
				// Ack
				if (typeof(outgoingMessages[json['ack']]) === 'object') {
					cancelTimeout(json['ack']);
				}
			} else if (typeof(json['result']) !== 'undefined') {
				// Response
				var tid = json['tid'];
				if (typeof(outgoingMessages[tid]) === 'object') {
					cancelTimeout(json['tid']);
					if (typeof(outgoingMessages[tid].callback) === 'function') {
						outgoingMessages[tid].callback(json);
					}
					delete outgoingMessages[tid];
				}
			} else if (json['cmd'] == 'event') {	
				// Server -> Client event
				log('NEW EVENT RECEIVED!', json);
				var eventName = 'core.event.' + json.data.class + '.' + json.data.subclass + '.' + json.data.type;
				dispatchEvent(eventName, json.data.obj_id, json.data.body);
			} else if (json['cmd'] == 'ping') {
				// Server -> Client call: Ping
				ws.send(JSON.stringify({
					result: 'ok',
					tid: json.tid
				}));
			} else {
				// Server -> Client call
				//TODO
				ws.send(JSON.stringify({
					result: 'ok',
					tid: json.tid
				}));
				error('Unhandled websocket call', json);
			}
		}

		function onWsError(event) {
			log('onWsError', event);
			dispatchEvent('onWsError', event);
		}
		
		function cancelTimeout(tid) {
			if (outgoingMessages[tid].timeout != null) {
				window.clearTimeout(outgoingMessages[tid].timeout);				
				outgoingMessages[tid].timeout = null;
			}
		}
		
		function cancelTimeoutsExcept(exceptTid) {
			for (var tid in outgoingMessages) {
				if (tid != exceptTid) {
					cancelTimeout(tid);
				}
			}
			outgoingMessages = [];
		}
		
		function cancelWsAliveTimeout() {
			if (wsAliveTimeout != null) {
				window.clearTimeout(wsAliveTimeout);
				wsAliveTimeout = null;
			}
		}
		
		function setWsAliveTimeout() {
			cancelWsAliveTimeout();
			wsAliveTimeout = window.setTimeout(function(){
				wsAliveTimeout = null;
				close();
			},5000);
		}

		/************************************/
		/** 			PROTOCOL		   **/
		/************************************/

		/**
		 * @command string
		 * @data Object
		 * @callback Object
		 * 
		 * This function send an object through the WebSocket. Once the response is received, callback.success or callback.error will be called
		 */
		function sendMessage(command, data, callback) {
			var tid = currentTid++;
			var now = new Date();
			var cmdData = {
				'cmd': command,
				'data': data,
				'tid': tid
			};
			outgoingMessages[tid] = {
				data: cmdData,
				date: now,
				timeout: window.setTimeout(function(){
					cancelTimeoutsExcept(tid);
					log('Timeout reached! at sendMessage');
					wsClose();
				},5000)
			};
			if (typeof(callback) === 'function') {
				outgoingMessages[tid].callback = callback;
			}
			ws.send(JSON.stringify(cmdData));
		}

		/**
		 * @command string
		 * @data Object
		 * 
		 * This function send an object through the WebSocket, and it will return a Promise. This promise will be settled once the response is received (either fullfilled or rejected, depending on the result received)
		 */
		function sendMessageAsync(command, data) {
			return new Promise(function(resolve, reject) {
				sendMessage(command, data, function(response) {
					if (response.result === 'ok') {
						resolve(response);
					} else {
						reject(response);
					}
				});
			});
		}

		/************************************/
		/**             STATE              **/
		/************************************/
		function isConnected() {
			return connected;
		}

		/************************************/
		/**              LOG               **/
		/************************************/
		function log() {
			// Riot logging
			//guf.console.log.apply(guf.console, arguments);
			// Normal logging
			console.log.apply(this, arguments);
		}
		function debug() {
			// Riot logging
			//guf.console.debug.apply(guf.console, arguments);
			// Normal logging
			console.debug.apply(this, arguments);
		}

		function error() {
			// Riot logging
			//guf.console.error.apply(guf.console, arguments);
			// Normal logging
			console.error.apply(this, arguments);
		}

		return {
			ws : ws,
			connect : wsConnect,
			close : wsClose,
			getHost : getHost,
			getPort : getPort,
			isConnected : isConnected,
			sendMessage : sendMessage,
			sendMessageAsync : sendMessageAsync
		}
	})();
	//riot.observable(guf.websocket);

})();