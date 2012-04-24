private var nc:NetConnection;
private var ns_in:NetStream;
private var ns_out:NetStream;
import mx.controls.Alert;

[Bindable]
private var registerEnabled:Boolean = false;

[Bindable]
private var registerLabel:String = "Register";

[Bindable]
private var callEnabled:Boolean = true;

[Bindable]
private var callLabel:String = "Call";

[Bindable]
private var callingLabel:String = "";

[Bindable]
private var acceptEnabled:Boolean = false;

[Bindable]
private var acceptLabel:String = "Accept";

[Bindable]
private var declineEnabled:Boolean = false;

[Bindable]
private var declineLabel:String = "Decline";

private var speexWB:Boolean = true;
private var inCall:Boolean = false;
private var inTalk:Boolean = false;

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;

  if(ExternalInterface.available) {
      log("External Interface available");
      ExternalInterface.addCallback("callToggle", outgoingCallToggle);
  } else {
      log("External Interface not available");
  }


  nc = new NetConnection();
  nc.client = this;
  nc.addEventListener(NetStatusEvent.NET_STATUS, handleStatus);
  nc.connect(Application.application.parameters.server+"/phone");
  if(Application.application.parameters.speex) {
    speexWB = Application.application.parameters.speex == "16000";
  }
}

private function handleStatus(evt:NetStatusEvent) : void
{
  var obj:Object = evt.info.description;
  switch(evt.info.code) {
    case "NetConnection.Connect.Success":
      registerEnabled = true;
      break;

    case "NetConnection.SipCall":
      ns_out = new NetStream(nc);

      var m:Microphone;
      /*      if (m['getEnhancedMicrophone'] && false) {
              m = Microphone.getEnhancedMicrophone();

              var options:MicrophoneEnhancedOptions = new MicrophoneEnhancedOptions();
              options.mode = MicrophoneEnhancedMode.FULL_DUPLEX;
              options.autoGain = false;
              options.echoPath = 128;
              options.nonLinearProcessing = true;
              m.enhancedOptions = options;
              } else {
              m = Microphone.getMicrophone();
              }
      */

      m = Microphone.getMicrophone();

      if(!m) {
        Alert.show("Cannot enable microphone");
      }
      var cam:Camera;
      cam = Camera.getCamera();
      cam.setMode(320, 240, 15);
      cam.setQuality(0, 90);


      //m.rate = 44;
      m.codec = "Speex";
      m.gain = 80;
      m.rate = speexWB ? 16000 : 8000;
      m.framesPerPacket = 1;
      ns_out.attachAudio(m);
      ns_out.attachCamera(cam);
      ns_out.publish(obj.out_stream);



      ns_in = new NetStream(nc);
      ns_in.play(obj.in_stream);
      //videoContainer.video.attachNetStream(ns_in);
      var in_client:Object = new Object();
      in_client.onMetaData = function(obj:Object):void {
      }
      ns_in.client = in_client;
      inTalk = true;
      declineEnabled = true;
      declineLabel = "Bye";
      break;

    case "NetConnection.IncomingCall":
      callingLabel = obj.calling_id;
      inTalk = false;
      acceptEnabled = true;
      declineEnabled = true;
      acceptLabel = "Accept";
      declineLabel = "Decline";
      break;

    case "NetConnection.Bye":
      ns_out.attachAudio(null);
      ns_out.attachCamera(null);
      inTalk = false;
      acceptEnabled = false;
      declineEnabled = false;
      callEnabled = true;
      acceptLabel = "Accept";
      declineLabel = "Decline";
      callLabel = "Call";
      break;

    default:
      registerEnabled = false;
      callEnabled = false;
      break;
  }
}

public function onMetaData(object:Object) : void
{

}

// public function unregister() : void
// {
//   nc.call("unregister", null, registerNumber.text);
// }

// public function bye() : void
// {
//   nc.call("bye", null);
// }

// public function register() : void
// {
//   var r:Responder = new Responder(function(reply:Boolean):void {
//     if(reply) {
//       registerLabel = "Registered";
//       callEnabled = true;
//     } else {
//       registerLabel = "Failed to register";
//     }
//   });
//   nc.call("register", r, registerNumber.text, registerPassword.text);
// }

public function outgoingCallToggle() : void
{
    if (inCall) {
        inCall = false;
        declineCall();
    } else {
        inCall = true;
        outgoingCall();
    }
}

private function outgoingCall() : void
{
  var r:Responder = new Responder(function(reply:Boolean):void {
      if(reply) {
        callLabel = "Calling";
      } else {
        callLabel = "Failed to call";
      }
    });
  inTalk = false;
  callEnabled = true;
  //declineEnabled = true;
  //declineLabel = "Cancel";
  callLabel = "Cancel";
  //nc.call("outgoingCall", r, callNumber.text);
  log("Call: "+Application.application.parameters.call_number);
  nc.call("outgoingCall", r, Application.application.parameters.call_number);
}

public function acceptCall() : void
{
  var r:Responder =
    new Responder(function(reply:Boolean):void {
        if(reply) {
          acceptLabel = "InTalk";
        } else {
          acceptLabel = "Failed to accept";
        }
      });
  inTalk = true;
  acceptEnabled = false;
  declineEnabled = true;
  declineLabel = "Bye";
  nc.call("acceptCall", r);
}

public function declineCall() : void
{
  var r:Responder =
    new Responder(function(reply:Boolean):void {
        if(reply) {
          declineLabel = "Declined";
        } else {
          declineLabel = "Failed to decline";
        }
      });
  inTalk = false;
  acceptEnabled = false;
  declineEnabled = false;
  callEnabled = true;
  callLabel = "Call";
  callingLabel = "";
  nc.call("declineCall", r);
}

private function log(s:String) : void {
  ExternalInterface.call("Phone.log", "[FLASH]: "+s);
}
