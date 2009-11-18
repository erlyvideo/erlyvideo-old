netconnection = {};
netconnection.connected = function() {
	if (console) {
		console.log("Connected");
	}
}

netconnection.failed = function() {
	if (console) {
		console.log("Disconnected");
	}
}

netconnection.message = function(message) {
  $("#chat").append("<div>"+message+"</div>");
}
