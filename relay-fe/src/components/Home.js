import React, { useState, useContext, useEffect } from 'react';
import '../App.css';
import { Button as SUIButton, Form, Message, Icon, List, Input, Label } from 'semantic-ui-react';
import Button from '../wallet/components/shared/Button';
// import { Wallet } from '../../../wallet/Wallet.js'
import { PactContext } from "../contexts/PactContext";
import { WalletContext } from "../wallet/contexts/WalletContext"
import SimpleSign from './SimpleSign'
import Relay from './Relay'

function Home() {
  const pact = useContext(PactContext);
  const wallet = useContext(WalletContext);
  const network = wallet.NETWORK_ID === "mainnet01" ? "mainnet" : "testnet";
 
  const {requestState, requestKey, response, localRes, error} = pact;
  const [key, setKey] = useState("");
  const [bond, setBond] = useState("");
  const [bondExist, setBondExist] = React.useState(false);
  const [publicKeys, setPublicKeys] = useState([]);
  const totalBonded = pact.tvl;

  useEffect(()=> {
    setKey("")
    if(wallet.account.guard && wallet.account.guard.keys){
      for (let i=0;i<wallet.account.guard.keys.length;i++){
        if (!publicKeys.includes(wallet.account.guard.keys[i])){
          setKey(wallet.account.guard.keys[i])
        }
      }
    }
  }, [wallet.account.account, publicKeys])

  const loading = (reqKey) => {
      return (
        <div>
          <p>
            <br/>
            {reqKey}
            <br/>
            <br/>
            Listening for result...
            <br/>
            <Icon loading name='circle notch'/>
          </p>
        </div>
      )
    }


   const renderRes = (res) => {
     if (!res) {
       return {
         header: "Result",
         content: JSON.stringify(res),
         hidden: false,
         warning: true
       }
     }
     else if (res.result && res.result.status === "failure") {
        return {
          header: "Result: Failure",
          content: <div><br/>
                     <p><b>Request Key:</b> {res.reqKey}</p>
                     <p><b>Block Height:</b> {res.metaData.blockHeight}</p>
                     <p><b>Block Hash:</b> {res.metaData.blockHash}</p>
                     <p><b>Result:</b> {JSON.stringify(res.result.error.message)}</p>
                   </div>,
          hidden: false,
          error: true,
          warning: true
        }
      } else if (res.result && res.result.status === "success"){
        return {
          header: "Result: Success",
          content: <div><br/>
                     <p><b>Request Key:</b> {res.reqKey}</p>
                     <p><b>Block Height:</b> {res.metaData.blockHeight}</p>
                     <p><b>Block Hash:</b> {res.metaData.blockHash}</p>
                     <p><b>Result:</b> {JSON.stringify(res.result.data)}</p>
                     <p>Check Your TX <a href={`https://explorer.chainweb.com/${network}/tx/${res.reqKey}`}><b>here</b></a></p>
                   </div>,
          hidden: false,
          success: true
        }
      }
    }

  const status = () => {
    const requestContent = {
      0: {header: "", content: "", hidden: true},
      1: {header: "Sign your Wallet", content: <p>
        1. In the Chainweaver popup, press 'Next'. <br/>2. Select public key to sign the transaction.<br/>3. Then press 'Next' and 'Submit'"</p>, hidden: false},
      2: {header: "Sign Completed", content: requestKey, hidden: false},
      3: {header: "Sending TX" , content: requestKey, hidden: false},
      4: {header: "Request Key", content: loading(requestKey), hidden: false},
      5: renderRes(response),
      6: {header: "Error", content: JSON.stringify(error), error:true, hidden: false},
      7: {header: "Preview: Failure", content: JSON.stringify(localRes), error: true, hidden: false},
      8: {header: "Preview: Success", content: JSON.stringify(localRes), error: false, hidden: false}
    }
    return requestContent[requestState];
  }

    return (
      <div className="App">
        <h5
          style={{ color:"yellow", position:"fixed", top:15, left: "5%", textAlign: "center", fontSize: 16 }}
        >
         {`${totalBonded/1000000} MM KDA TVL`}
        </h5>
        <header className="App-header">
          <img src={require("../kadena.png")} style={{height:100, marginBottom: 10}}/>
          <h1>
            Kadena {network === "mainnet" ? "" : "Testnet"} Chain Relay
          </h1>
          <h5>Create and manage Kadena Chain Relay Bonds {network === "mainnet" ? "" : "on Testnet"}
          </h5>

          <Form
            success={status().success}
            error={status().error}
            warning={status().warning}
            inverted
          >

            <Form.Field
              style={{marginTop: "10px", marginBottom: 5, width: "360px", marginLeft: "auto", marginRight: "auto"}}
              >
              <label style={{color: "#18A33C", marginBottom: 5, textAlign: "left", width: "360px", }}>
                Create a New Bond
              </label>
              <Input
                value={key}
                error={wallet.account.guard && wallet.account.guard.keys.includes(key)}
                style={{width: "360px"}}
                icon='key'
                iconPosition='left'
                placeholder='Bond Guard (Enter Public Key)'
                value={key}
                onChange={(e) => setKey(e.target.value)}
                action= {
                  <SUIButton
                    disabled={key.length  !== 64 || publicKeys.indexOf(key)!==-1 || (false && wallet.account.guard && wallet.account.guard.keys.includes(key))}
                    icon="add"
                    onClick={() => {
                      setPublicKeys([...publicKeys, key])
                      setKey("")
                    }}
                  />
                }
              />
              {(false && wallet.account.guard && wallet.account.guard.keys.includes(key))
                ?
                <Label pointing color="red" hidden >
                  Please use a key that is not used in your Kadena account
                </Label>
                : ""
              }
              <List celled style={{overflowX: "auto"}}>
              {publicKeys.map(item =>  <List.Item icon='key' style={{color: "white"}} content={item} key={item}/>)}
             </List>
            </Form.Field>

            <Form.Field style={{marginTop: -10, marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}  >
              <Button
                disabled={wallet.account.account === "" || wallet.account.account === null || publicKeys.length === 0}
                style={{
                  backgroundColor: "#18A33C",
                  color: "white",
                  width: 360,
                }}
                onClick={() => {
                  pact.newBond(wallet.account.account, publicKeys)
                }}
              >
                New Bond
              </Button>
            </Form.Field>

            <Form.Field  style={{ marginTop: "20px", marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}} >
            <label style={{color: "#18A33C", marginBottom: 5, textAlign: "left" }}>
              Unbond / Renew Bond
            </label>
              <Form.Input
                style={{ width: "360px" }}
                icon='money bill alternate outline'
                iconPosition='left'
                placeholder='Bond Name'
                value={bond}
                onChange={async e => {
                  setBond(e.target.value)
                  let res = await pact.getBond(e.target.value);
                  if (res) setBondExist(true);
                }}
              />
            </Form.Field>
            <Form.Field style={{marginTop: 10, marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}  >
              <SimpleSign
                disabled={bond === ""}
                activity="Unbond"
                bond={bond}
                bondExist={bondExist}
              />
              <SimpleSign
                disabled={bond === ""}
                activity="Renew"
                bond={bond}
                bondExist={bondExist}
              />
            </Form.Field>

            <Form.Field style={{width: 670, margin: "auto"}}>
              <Message
                success={status().success}
                warning={status().warning}
                error={status().error}
                hidden={status().hidden}
              >
                <Message.Header>{status().header}</Message.Header>
                <div>{status().content}</div>
                  <Message hidden={requestState!==8}>
                    <Button
                      disabled={status().error}
                      style={{
                        marginTop: 10
                      }}
                      onClick={() => pact.sendCmd()}
                    >
                      Confirm
                    </Button>
                  </Message>
              </Message>
            </Form.Field>
          </Form>
          <Relay/>
        </header>
      </div>)
}

export default Home;
