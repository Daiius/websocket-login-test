
export class ConnectionManager
{
  ws: WebSocket | null;
  nretry: number;
  name: string;

  constructor() {
    this.ws = null;
    this.nretry = 0;
    this.name = "";
  }


  wait = async(ms: number) => new Promise(resolve => setTimeout(resolve, ms));

  connect = async(name: string) => {
    console.log(name);
    this.name = name;

    if (this.ws) {
      console.log("Already connected. closing...");
      this.close()
      await this.wait(2000);
    }

    console.log("Create socket...");
    this.ws = new WebSocket("ws://" + window.location.host);
    this.ws.onopen = this.onOpen;
    this.ws.onclose = this.onClose;
    this.ws.onmessage = this.onMessage;
    this.ws.onerror = (e) => console.log(e);
    
  }

  close = () => {
      

      this.ws!.onclose = null;
      this.ws!.onopen = null;
      this.ws!.onmessage = null;
      this.ws!.close();
      console.log("Close websocket connection.");
      this.ws = null;
  }

  onOpen = () => {
    console.log("open event");
    this.ws!.send("name," + this.name);
    this.nretry = 0;
  }

  onClose = async(e: CloseEvent) => {
    console.log("Connection closed, code: " + e.code);
    this.close();

    try {
      console.log("Trying to reconnect..., try count: " + this.nretry);
      this.nretry += 1;
      if (this.nretry > 20) {
        console.log("Reconnection max try count reached. failed.");
        return;
      }
      await this.wait(1000);
      this.connect(this.name);
    } catch (err) {
      console.error(err);
    }
  }

  onMessage = (e: MessageEvent) => {
    console.log("Message received: " + e.data);
  }
}
