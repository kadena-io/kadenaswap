import React, {useContext}  from 'react';
import Pact from 'pact-lang-api';
import { Icon } from 'semantic-ui-react'
const Context = React.createContext();
import {WalletContext} from "./wallet/contexts/WalletContext"
const creationTime = () => Math.round((new Date).getTime()/1000)-10;
const GAS_PRICE = 0.000000000001;
const BOND_AMOUNT = 50000;
const CHAIN_ID = "0";
const NETWORKID = 'testnet04';
const APIHost = `https://api.testnet.chainweb.com/chainweb/0.0/${NETWORKID}/chain/${CHAIN_ID}/pact`

export class PactStore extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      bondAccount: "",
      reqKey: "",
      requestState: 0,
      response: "",
      error: ""
    };
  }

  renderRes = (res) => {
    if (res.result && res.result.status === "failure") {
      return {
        header: "Result: Failure",
        content: <div><br/>
                   <p><b>Request Key:</b> {res.reqKey}</p>
                   <p><b>Block Height:</b> {res.metaData.blockHeight}</p>
                   <p><b>Block Hash:</b> {res.metaData.blockHash}</p>
                   <p><b>Result:</b> {JSON.stringify(res.result.error.message)}</p>
                 </div>,
        hidden: false,
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
                   <p>Check Your TX <a href="https://balance.chainweb.com"><b>here</b></a></p>
                 </div>,
        hidden: false,
        success: true
      }
    } else {
      return {
        header: "Result",
        content: JSON.stringify(res),
        hidden: false,
        warning: true
      }
    }
  }

  loading = (reqKey) => {
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

  status = () => {
    const requestContent = {
      0: {header: "", content: "", hidden: true},
      1: {header: "Sign your Wallet", content: <p>
        1. In the Chainweaver popup, press 'Next'. <br/>2. Select public key to sign the transaction.<br/>3. Then press 'Next' and 'Submit'"</p>, hidden: false},
      2: {header: "Sign Completed", content: this.state.reqKey, hidden: false},
      3: {header: "Sending TX" , content: this.state.reqKey, hidden: false},
      4: {header: "Request Key", content: this.loading(this.state.reqKey), hidden: false},
      5: this.renderRes(this.state.response),
      6: {header: "Error", content: this.state.error, error:true, hidden: false}
    }
    return requestContent[this.state.requestState];
  }


  getBond = async (bond) => {
    const cmd = {
        pactCode: `(relay.pool.get-bond (read-msg 'bond))`,
        envData: {
          bond: bond
        },
        meta: Pact.lang.mkMeta('relay-free-gas', CHAIN_ID,1000,5000,creationTime(), 600),
        networkId: 'testnet04'
      };

    let res = await Pact.fetch.local(cmd, APIHost);
    if (res.result.status === "success"){
      this.setState({bondAccount: res.result.data.account})
    } else {
      this.setState({requestState: 6, error: "Bond is not available"})
      this.setState({bondAccount: ""})
    };
    return this.state.bondAccount
  }

  newBond = async (acct,key) => {

    const cmd = {
        pactCode: `(relay.pool.new-bond relay.relay.POOL (read-msg 'account) (read-keyset 'ks))`,
        caps: [
          Pact.lang.mkCap("Gas Station", "free gas", "relay.gas-station.GAS_PAYER", ["free-gas", {int: 1}, 1.0]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `coin.TRANSFER`, [acct, "relay-bank", BOND_AMOUNT]),
        ],
        sender: 'relay-free-gas',
        gasLimit: 2000,
        gasPrice: GAS_PRICE,
        chainId: CHAIN_ID,
        ttl: 1500,
        envData: {
          account: acct,
          ks: [key]
        }
      }
    this.sendBond(cmd);
  }

  unBond = async (acct, bond) => {
    const cmd = {
        pactCode: `(relay.pool.unbond (read-msg 'bond))`,
        caps: [
          Pact.lang.mkCap("Gas Station", "free gas", "relay.gas-station.GAS_PAYER", ["free-gas", {int: 1}, 1.0]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `coin.TRANSFER`, ["relay-bank", acct, BOND_AMOUNT]),
          Pact.lang.mkCap("Bonder", "Bond", "relay.pool.BONDER", [bond])
        ],
        sender: 'relay-free-gas',
        gasLimit: 2000,
        gasPrice: GAS_PRICE,
        chainId: CHAIN_ID,
        ttl: 1500,
        envData: {
          bond: bond
        }
      }
    this.sendBond(cmd);
  }

  renewBond = async (bond) => {
    const cmd = {
        pactCode: `(relay.pool.renew (read-msg 'bond))`,
        caps: [
          Pact.lang.mkCap("Gas Station", "free gas", "relay.gas-station.GAS_PAYER", ["free-gas", {int: 1}, 1.0]),
          Pact.lang.mkCap("Bonder", "Bond", "relay.pool.BONDER", [bond])
        ],
        sender: 'relay-free-gas',
        gasLimit: 2000,
        gasPrice: GAS_PRICE,
        chainId: CHAIN_ID,
        ttl: 1000,
        envData: {
          bond: bond
        }
      }
    this.sendBond(cmd);
  }

  sendBond = async (signCmd) => {
    try {
        //Wallet Open
        this.setState({requestState: 1});
        Pact.wallet.sign(signCmd).then(cmd => {
          //Wallet Signed && request Sent
          this.setState({requestState: 2});
          return fetch(`${APIHost}/api/v1/send`, {
            headers: {
              "Content-Type": "application/json"
            },
            method: "POST",
            body: JSON.stringify({"cmds": [cmd]})
          });
        })
        .then(async res => {
          console.log(res)
          let reqKey
          if (res.ok){
            reqKey = await res.json();
          } else {
             let resTEXT = await res.text()
             console.log(resTEXT)
            throw resTEXT;
          }
          return reqKey
        }).then(reqKey => {
          //RequestKey Fetched
          this.setState({reqKey: reqKey.requestKeys[0], requestState: 3})
          return reqKey.requestKeys[0]
        }).then(reqKey => {
          //Listening for result
          this.setState({requestState: 4})
          return Pact.fetch.listen({"listen": reqKey }, APIHost)
        }).then(res => {
          //Result came back
          console.log(res)
          this.setState({requestState: 5, response: res})
          console.log(this.state.response)
          return res
        }).then(res => {
          this.setState({requestState: 5, response: res})
          console.log("why are you reloading")
        }).catch(e => {
          //Error
          console.log(e)
          if (e=== "Error in $.cmds[0]: parsing Command failed, expected Object, but encountered Null"){
            this.setState({requestState: 6, error: "Signing was unsuccessful"})
          } else if (typeof e === "object"){
            this.setState({requestState: 6, error: "Open your wallet"})
            console.log("error" +  e)
          } else this.setState({requestState: 6, error: e})
        })

    } catch(err){
      console.log(err);
      alert("you cancelled the TX or you did not have the wallet app open")
      // window.location.reload();
    }
  }

  render() {
    return (
      <Context.Provider
        value={{
          ...this.state,
          newBond: this.newBond,
          unBond: this.unBond,
          renewBond: this.renewBond,
          status: this.status,
          bondAccount: this.state.bondAccount
        }}
      >
        {this.props.children}
      </Context.Provider>
    );
  }

}

export default Context;
