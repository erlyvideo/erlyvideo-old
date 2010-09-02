private var nc:NetConnection;
private var ns_in:NetStream;
private var ns_out:NetStream;

[Bindable]
private var registerEnabled:Boolean = false;

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
  
  nc = new NetConnection();
  nc.client = this;
  nc.addEventListener(NetStatusEvent.NET_STATUS, handleStatus);
  nc.connect(Application.application.parameters.server+"/phone");
}

private function handleStatus(evt:NetStatusEvent) : void
{
  switch(evt.info.code) {
    case "NetConnection.Connect.Success":
      registerEnabled = true;
      break;
      
    case "NetConnection.SipCall":
      var obj:Object = evt.info.description;
      ns_out = new NetStream(nc);
      
      var m:Microphone = Microphone.getMicrophone();
			//m.rate = 44;
      m.codec = "Speex";
			m.gain = 80;
      ns_out.attachAudio(m);
      ns_out.publish(obj.out_stream);
      
      ns_in = new NetStream(nc);
      ns_in.play(obj.in_stream);
      
    default:
      registerEnabled = false;
      break;
  }
}

public function register() : void
{
  nc.call("register", null, number.text);
}