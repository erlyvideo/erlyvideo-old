/*
 * jsSO - Javascript Shared Objects
 * http://pro-web.at/projects/jsso
 *
 * Copyright (c) 2009 Daniel Prieler
 * Date: 2009-04-14
 * Version: 0.2
 */

(function($){

var jsSO = window.jsSO = {
	_defaultOptions: {
		debug: false,
		loadServices: true,
		persistent: false
	},
	options: {},
	_data: {},
	_updates: {},
	_calls: {},
	_lastCallbackId: 0,
	service: {},

	connect: function(serverUrl, objectName, options) {
		if (typeof objectName === 'object') {
			options = objectName;
			objectName = options.objectName;
		}
	
		if (objectName && objectName.match(/rtmps?:\/\//)) {
			this.debug('WARNING: connect parameter order has changed: connect(serverUrl, objectName)');
			var tmp = serverUrl;
			serverUrl = objectName;
			objectName = tmp;
		} else if (!serverUrl.match(/rtmps?:\/\//)) {
			this.error('wrong serverUrl given: '+serverUrl);
			return;
		}
		
		$.extend(this.options, this._defaultOptions, options);
	
		this.debug('connect');

		var flashvars = {
			objectName: objectName,
			serverUrl: serverUrl,
			persisent: this.options.persistent
		};
		var params = {};
		var attributes = {};

		$(function(){
			var $div = $('<div style="position: absolute; left: -1000px; top: -1000px;"><div id="jsSOSwf"></div></div>').appendTo('body');
			if (jsSO.options.debug) {
				$div.css({left: null, right:0, top:0});
			}
			swfobject.embedSWF('jsSO/jsSO.swf', "jsSOSwf", 320, 180, "9.0.0", false, flashvars, params, attributes);
		});
		
		return this;
	},
	fcbFlashInit: function() {
		this.debug('flashInit');
		$(this).trigger('flashInit.jsSO');
	},
	fcbNetStatus: function(code) {
		// possible errors:
		// http://livedocs.adobe.com/flash/9.0/ActionScriptLangRefV3/flash/events/NetStatusEvent.html
		if (code == 'NetConnection.Connect.Success') {
			this.debug('onConnect');
			
			if (this.options.loadServices) {
				this.reloadServices();
			} else {
				$(this).trigger('connect.jsSO');
			}
		} else if (code == 'NetConnection.Connect.AppShutdown' || code == 'NetConnection.Connect.Closed') {
			// server closed connection
			this.debug('onDisconnect: '+code);
			$(this).trigger('disconnect.jsSO', code);
		} else {
			// error while connecting
			this.debug('onConnectError: '+code);
			$(this).trigger('connectError.jsSO', code);
		}
	},
	fcbFlashError: function(type, message) {
		this.debug('fcbFlashError: '+type+' / '+message);
		$(this).trigger('flashError.jsSO', type, message);
	},

	debug: function(text) {
		if (this.options.debug && window.console && window.console.log) {
			window.console.log('[jsSO] '+text);
		}
	},
	error: function(text) {
		this.debug('ERROR: '+text);
	},
	_swf: function() {
		return $('#jsSOSwf')[0];
	}
};

// functions for shared objects
$.extend(jsSO, {
	fcbSync: function(updates, data) {
		this.debug('fcbSync');
		//this.debug(updates);
		//this.debug(data);
		this._data = data;
		this._updates = updates;
		
		// trigger onSync events
		$(this).trigger('sync.jsSO', [updates, data]);
	},

	fcbOnMessage: function(data) {
		this.debug('fcbOnMessage');
		//this.debug(updates);
		//this.debug(data);
		this._data = data;
		
		// trigger onSync events
		$(this).trigger('message.jsSO', [data]);
	},

	// set a data item
	set: function(item, data) {
		this._swf().set(item, data);
		return this;
	},

	send: function(name, message) {
		this._swf().send(name, message);
		return this;
	},

	// data and updates
	getData: function() {
		return this.data;
	},
	getUpdates: function() {
		return this.updates;
	},
});

// functions for calling server-side functions
$.extend(jsSO, {
	reloadServices: function(){
		this.services = {};
		this.call('getServiceList').addCallback(function(serviceList){
			$.each(serviceList, function(i, func) {
				jsSO.service[func] = function() {
					return jsSO.callArgsArray(func, $.makeArray(arguments));
				};
			});
			$(this).trigger('connect.jsSO', this.services);
		});
	},
	call: function(func, arg1, arg2) {
		return this.callArgsArray(func, Array.prototype.slice.call(arguments, 1));
	},
	callArgsArray: function(func, args) {
		var callback = null;
		var errorCallback = null;
		
		if ($.isFunction(args[args.length-1])) {
			callback = args.pop();
			if ($.isFunction(args[args.length-1])) {
				errorCallback = callback;
				callback = args.pop();
			}
		}
		
		this._lastCallbackId++;
		var call = this._calls[this._lastCallbackId] = {
			func: func,
			callback: callback || function(){},
			errorCallback: errorCallback || function(){},
			addCallback: function(callback){
				this.callback = callback;
				return this;
			},
			addErrback: function(callback){
				this.errorCallback = callback;
				return this;
			}
		};
		
		this._swf().call(this._lastCallbackId, func, args);
		
		return call;
	},
	fcbCallResponse: function(id, response) {
		var call = this._calls[id];
		if (!call) {
			throw new Exception('call not found!');
		}
		
		this.debug('fcbCallResponse for '+call.func);
		$(this).trigger('callResponse.jsSO', call, response);

		call.callback.apply(this, [response]);
		delete this.callbacks[id];
	},
	fcbCallError: function(id, error) {
		var call = this._calls[id];
		if (!call) {
			throw new Exception('call not found!');
		}

		this.debug('fcbCallError for '+call.func+': '+error.description);
		$(this).trigger('callError.jsSO', call, error);

		call.errorCallback.apply(this, [error]);
		delete this.callbacks[id];
	}
});

// events
$.each(('connect,connectError,disconnect,sync,message,flashError,callResponse,callError').split(','), function(i, name) {
	jsSO['on'+name.charAt(0).toUpperCase()+name.substr(1)] = function(func) {
		$(this).bind(name+'.jsSO', func);
	};
});

})(jQuery);
