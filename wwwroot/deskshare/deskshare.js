function deskshareStart(fullScreen) {
  startApplet(window.location.hostname, window.conference, fullScreen);
  
}

function startApplet(IP, roomNumber, fullScreen)
{
	var iframe = document.createElement("iframe");
  	iframe.id = "iframe";
   	document.body.appendChild(iframe);
   	frames[frames.length - 1].document.write(
   		"<applet code=\"org.bigbluebutton.deskshare.client.DeskShareApplet.class\"" +
   			"id=\"DeskShareApplet\" width=\"40\" height=\"40\" archive=\"/deskshare/bbb-deskshare-applet-0.71.jar\">" +
        	"<param name=\"ROOM\" value=\"" + roomNumber  + "\"/>" +
        	"<param name=\"IP\" value=\"" + IP + "\"/>" +
        	"<param name=\"HTTP_TUNNEL\" value=\"true\"/>" +
        	"<param name=\"FULL_SCREEN\" value=\"" + fullScreen + "\"/>" +       	
      	"</applet>"
     );
}
