import React, { useState, useContext, createContext }  from 'react';
import Pact from 'pact-lang-api';
import { WalletContext } from "../wallet/contexts/WalletContext"

const BOND_AMOUNT = 50000;
const CHAIN_ID = "0";
const NETWORK_ID = 'testnet04';

export const PactContext = createContext();

export const PactProvider = (props) => {
  const [bondAccount, setBondAccount] = useState("");
  const [requestKey, setRequestKey] = useState("");
  const [requestState, setRequestState] = useState(0);
  const [response, setResponse] = useState("");
  const [error, setError] = useState("");
  const [transaction, setTransaction] = useState(null);
  const [localRes, setLocalRes] = useState(null);

  let wallet = useContext(WalletContext);
  const {
    creationTime, apiHost, signing, GAS_PRICE, account, privKey,
    pwPrompt,
    walletError,
    walletSigError,
    walletLoading,
    decryptKey
  } = wallet;

  const newBond = async (acct,key) => {
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
    if (signing.method === "sign"){
       sendBondWallet(cmd);}
    else {
      sendBondLocal(cmd);
    }
  }

  const unBond = async (acct, bond) => {
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
      if (signing.method === "sign") sendBondWallet(cmd);
      else {
        sendBondLocal(cmd);
      }
    }

  const renewBond = async (bond) => {
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
      if (signing.method === "sign") sendBondWallet(cmd);
      else {
        sendBondLocal(cmd);
      }
    }

  const sendBondLocal = async (signCmd) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = await pwPrompt();
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return
      }
      const cmd = {
          pactCode: signCmd.pactCode,
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: signCmd.caps.map(cap => {
              return cap.cap;
            })
          },
          envData: signCmd.envData,
          meta: Pact.lang.mkMeta(signCmd.sender, CHAIN_ID, GAS_PRICE, signCmd.gasLimit, creationTime(), signCmd.ttl),
          networkId: NETWORK_ID
      }
      let data = await Pact.fetch.local(cmd, apiHost(NETWORK_ID, CHAIN_ID));
      setTransaction(cmd);
      if (data.result.status === "success"){
        setLocalRes(data.result.data);
        setRequestState(8);
      } else {
        setLocalRes(data.result.error.message);
        setRequestState(7);
      }
      return data;
    } catch (e) {
      setLocalRes({});
      return -1
    }
  }

  const sendCmd = async () => {
    setRequestState(2);
    Pact.fetch.send(transaction, apiHost(NETWORK_ID, CHAIN_ID))
      .then(reqKey => {
        //RequestKey Fetched
        setRequestKey(reqKey.requestKeys[0])
        setRequestState(3);
        return reqKey.requestKeys[0]
      }).then(reqKey => {
        //Listening for result
        setRequestState(4);
        return Pact.fetch.listen({"listen": reqKey }, apiHost(NETWORK_ID, CHAIN_ID))
      })
      .then(res => {
        //Result came back
        setRequestState(5);
        setResponse(res);
        return res
      })
      .then(res => {
        setRequestState(5);
        setResponse(res);
      })
      .catch(e => {
        //Error
        setRequestState(6);
        setError(e);
      })
  }

  const sendBondWallet = async (signCmd) => {
      //Wallet Open
      setRequestState(1);
      try {
          //Sign wallet
          Pact.wallet.sign(signCmd)
            .then(cmd => {
              if (!cmd) {
                throw "Signing was unsuccessful"
              }
              else {
                //send cmd to node
                setRequestState(2);
                return fetch(`${apiHost(NETWORK_ID, CHAIN_ID)}/api/v1/send`, {
                  headers: {
                    "Content-Type": "application/json"
                  },
                  method: "POST",
                  body: JSON.stringify({"cmds": [cmd]})
                });
              }
            })
            .then(async res => {
              let reqKey
              if (res.ok){
                reqKey = await res.json();
                return reqKey
              } else {
                 //Wallet closed without signing
                let resTEXT = await res.text()
                throw resTEXT;
              }
          })
          .then(reqKey => {
            //RequestKey Fetched
            setRequestKey(reqKey.requestKeys[0])
            setRequestState(3);
            return reqKey.requestKeys[0]
          }).then(reqKey => {
            //Listening for result
            setRequestState(4);
            return Pact.fetch.listen({"listen": reqKey }, apiHost(NETWORK_ID, CHAIN_ID))
          }).then(res => {
            //Result came back
            setRequestState(5);
            setResponse(res);
            return res
          }).then(res => {
            setRequestState(5);
            setResponse(res);
          }).catch(e => {
            //Error
            if (e=== "Error in $.cmds[0]: parsing Command failed, expected Object, but encountered Null"){
              setRequestState(6);
              setError("Signing was unsuccessful");
            } else if (typeof e === "object"){
              setRequestState(6);
              setError("Open your wallet");
            } else {
              setRequestState(6);
              setError(e);
            }
          })

      } catch(err){
        alert("you cancelled the TX or you did not have the wallet app open")
      }

    }

    return (
      <PactContext.Provider
        value={{
          newBond,
          unBond,
          renewBond,
          bondAccount,
          requestState,
          error,
          requestKey,
          localRes,
          response,
          sendCmd
        }}
      >
        {props.children}
      </PactContext.Provider>
    );
}

export const PactConsumer = PactContext.Consumer;

export const withPactContext = (Component) => (props) => (
  <PactConsumer>{(providerProps) => <Component {...props} sessionContextProps={providerProps} />}</PactConsumer>
);
