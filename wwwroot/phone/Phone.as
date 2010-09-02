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
  nc.addEventListener(NetStatusEvent.NET_STATUS, handleStatus);
  nc.connect(Application.application.parameters.server+"/phone");
}

private function handleStatus(evt:NetStatusEvent) : void
{
  switch(evt.info.code) {
    case "NetConnection.Connect.Success":
      registerEnabled = true;
      break;
    default:
      registerEnabled = false;
      break;
  }
}

public function register() : void
{
  nc.call("register", null, number.text);
}