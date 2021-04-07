import React, { useState, useContext } from 'react';
import '../App.css';
import { Button, Form, Message } from 'semantic-ui-react';
import { Wallet } from './wallet/Wallet.js'
import PactContext from "../contexts/PactContext";
import {WalletContext} from "./wallet/contexts/WalletContext"

function Home() {

  const pactContext = useContext(PactContext);
  const wallet = useContext(WalletContext);
  const result = pactContext.status
  const [key, setKey] = useState("");
  const [bond, setBond] = useState("");
  console.log(wallet.account.account)
  return (
    <div className="App">
      <Wallet/>
      <header className="App-header">
        <img src={require("../kadena.png")} style={{height:100, marginBottom: 10}}/>
        <h1>
          Kadena Testnet Chain Relay
        </h1>
        <h5>Create and manage Kadena Chain Relay Bonds on Testnet
        </h5>

        <Form success={result().success}
              error={result().error}
              warning={result().warning}>

          <Form.Field  style={{marginTop: "0px", marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}} >
            <label style={{color: "#18A33C", textAlign: "left" }}>
              Create a New Bond
            </label>
            <Form.Input
              style={{width: "360px"}}
              icon='user'
              iconPosition='left'
              placeholder='Bond Guard (Enter Public Key)'
              value={key}
              onChange={(e) => setKey(e.target.value)}
            />
          </Form.Field>

          <Form.Field style={{marginTop: 10, marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}  >
            <Button
              disabled={wallet.account === "" || key === ""}
              style={{
              backgroundColor: "#18A33C",
              color: "white",
              width: 360,
              }}
              onClick={() => {
                pactContext.newBond(wallet.account.account, key)}}
            >
              New Bond
            </Button>
          </Form.Field>

          <Form.Field  style={{marginTop: "0px", marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}} >
          <label style={{color: "#18A33C", textAlign: "left" }}>
            Unbond / Renew Bond
          </label>
            <Form.Input
              style={{width: "360px"}}
              icon='user'
              iconPosition='left'
              placeholder='Bond Name'
              value={bond}
              onChange={(e) => setBond(e.target.value)}
            />
          </Form.Field>

          <Form.Field style={{marginTop: 10, marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}  >
            <Button
              disabled={bond === ""}
              style={{
              backgroundColor: "#18A33C",
              color: "white",
              width: 360,
              }}
              onClick={() => pactContext.unBond(wallet.account.account, bond)}
            >
              Unbond
            </Button>
          </Form.Field>
          <Form.Field style={{marginTop: 10, marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}  >
            <Button
              disabled={bond === ""}
              style={{
              backgroundColor: "#18A33C",
              color: "white",
              width: 360,
              }}
              onClick={() => pactContext.renewBond(bond)}
            >
              Renew
            </Button>
          </Form.Field>

          <Form.Field style={{width: 500, margin: "auto"}}>
            <Message
              success={result().success}
              warning={result().warning}
              error={result().error}
              hidden={result().hidden}
            >
              <Message.Header>{result().header}</Message.Header>
              <div>{result().content}</div>
            </Message>
          </Form.Field>
        </Form>
      </header>
    </div>
  );
}

export default Home;
