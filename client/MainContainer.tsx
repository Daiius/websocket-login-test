
import React, { useState } from "react";
import { ConnectionManager } from "./connection_manager";

export const MainContainer = () => {
  
  const [name, setName] = useState("");

  const handleNameChanged = (e: React.ChangeEvent<HTMLInputElement>) => setName(e.target.value);

  const connectionManager = new ConnectionManager();

  const handleConnectClicked = (e: React.MouseEvent<HTMLInputElement>) => connectionManager.connect(name);

  return (
    <div>
      <p>Hello, React!</p>
      <p>Name: {name}</p>
        <input type="text" autoFocus value={name} onChange={handleNameChanged}/>
        <input type="button" value="Connect" onClick={handleConnectClicked}/>
    </div>
    );
}

