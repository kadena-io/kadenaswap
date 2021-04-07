import React, { useState, createContext, useEffect, useContext } from 'react';
import Pact from "pact-lang-api";
import CryptoJS from 'crypto-js'
import { NotificationContext, STATUSES } from './NotificationContext';
import PasswordPopup from '../components/shared/PasswordPopup';
import { toast } from 'react-toastify';
import swal from '@sweetalert/with-react'
import pwPrompt from '../components/alerts/pwPrompt'
import walletError from '../components/alerts/walletError'
import walletSigError from '../components/alerts/walletSigError'
import walletLoading from '../components/alerts/walletLoading'

export const WalletContext = createContext();
const savedAcct = localStorage.getItem('acct');
const savedPrivKey = localStorage.getItem('pk');
const savedNetwork = localStorage.getItem('network');
const savedSlippage = localStorage.getItem('slippage');
const savedSigning = localStorage.getItem('signing');
const savedTtl = localStorage.getItem('ttl');
const chainId = "0";
const PRECISION = 12;
const NETWORKID = 'testnet04';
const network = `https://api.testnet.chainweb.com/chainweb/0.0/${NETWORKID}/chain/${chainId}/pact`;

const creationTime = () => Math.round((new Date).getTime()/1000)-10;
const GAS_PRICE = 0.000000000001;

export const WalletProvider = (props) => {
  const notificationContext = useContext(NotificationContext);
  const [account, setAccount] = useState((savedAcct ? JSON.parse(savedAcct) : {account: null, guard: null, balance: 0}));
  const [tokenAccount, setTokenAccount] = useState({account: null, guard: null, balance: 0});
  const [privKey, setPrivKey] = useState((savedPrivKey ? savedPrivKey : ""));
  const keyPair = privKey ? Pact.crypto.restoreKeyPairFromSecretKey(privKey) : "";
  const [cmd, setCmd] = useState(null);
  const [localRes, setLocalRes] = useState(null);
  const [polling, setPolling] = useState(false);
  const [totalSupply, setTotalSupply] = useState("")
  const [poolBalance, setPoolBalance] = useState(["N/A", "N/A"]);
  const [sendRes, setSendRes] = useState(null);
  const [signing, setSigning] = useState(savedSigning ? JSON.parse(savedSigning) : { method: 'none', key: "" });
  const [sigView, setSigView] = useState(false);
  const [pw, setPw] = useState("");
  const [pwStatus, setPwStatus] = useState("");
  const [walletSuccess, setWalletSuccess] = useState(false);
  const [registered, setRegistered] = useState(false);
  const [ttl, setTtl] = useState((savedTtl ? savedTtl : 600));
  const [balances, setBalances] = useState(false);

  //TO FIX, not working when multiple toasts are there
  const toastId = React.useRef(null)
  // const [toastIds, setToastIds] = useState({})
  useEffect(() => {
    if (account.account) setRegistered(true);
  }, [registered]);

  useEffect(() => {
    if (account.account) setVerifiedAccount(account.account);
  }, [sendRes])

  useEffect(() => {
    const store = async () => localStorage.setItem('signing', JSON.stringify(signing));
    store()
  }, [signing])

  const pollingNotif = (reqKey) => {
    return (
      toastId.current = notificationContext.showNotification({
              title: 'Transaction Pending',
              message: reqKey,
              type: STATUSES.INFO,
              autoClose: 92000,
              hideProgressBar: false
            }
      )
    )
  }

  const getCorrectBalance = (balance) => {
    const balanceClean = (!isNaN(balance) ? balance : balance.decimal)
    return balanceClean
  }

  const setVerifiedAccount = async (accountName) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(coin.details ${JSON.stringify(accountName)})`,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          await localStorage.setItem('acct', JSON.stringify(data.result.data));
          setAccount({...data.result.data, balance: getCorrectBalance(data.result.data.balance)});
          await localStorage.setItem('acct', JSON.stringify(data.result.data));
        } else {
          setAccount({account: null, guard: null, balance: 0});
        }
    } catch (e) {
      console.log(e)
    }
  }

  const wait = async (timeout) => {
    return new Promise(resolve => {
        setTimeout(resolve, timeout);
    });
  }

  const listen = async (reqKey) => {
    //check kadena tx status every 10 seconds until we get a response (success or fail)
    var time = 240;
    var pollRes;
    while (time > 0) {
      await wait(5000);
      pollRes = await Pact.fetch.poll({requestKeys: [reqKey]}, network);
      if (Object.keys(pollRes).length === 0) {
        console.log('no return poll');
        console.log(pollRes)
        time = time - 5
      } else {
        console.log(pollRes);
        time = 0;
      }
    }
    setSendRes(pollRes);
    console.log(reqKey)
    console.log(pollRes)
    console.log(pollRes[reqKey])
    console.log(pollRes[reqKey].result)
    if (pollRes[reqKey].result.status === 'success') {
      notificationContext.showNotification({
              title: 'Transaction Success!',
              message: 'Check it out in the block explorer',
              type: STATUSES.SUCCESS,
              onClose: async () => {
                await toast.dismiss(toastId)
                await window.open(
                  `https://explorer.chainweb.com/mainnet/tx/${reqKey}`,
                  "_blank",
                  'noopener,noreferrer'
                );
                window.location.reload()
              },
              onOpen: async (value) => {
                await toast.dismiss(toastId.current)
              }
            }
      )
    } else {
      notificationContext.showNotification({
              title: 'Transaction Failure!',
              message: 'Check it out in the block explorer',
              type: STATUSES.ERROR,
              onClose: async () => {
                await toast.dismiss(toastId)
                await window.open(
                  `https://explorer.chainweb.com/mainnet/tx/${reqKey}`,
                  "_blank",
                  'noopener,noreferrer'
                );
                window.location.reload()
              },
              onOpen: async (value) => {
                await toast.dismiss(toastId.current)
              }
            }
      )
    }
  }

  const clearSendRes = () => {
    setVerifiedAccount(account.account)
    setSendRes(null);
  }

  const storePrivKey = async (pk) => {
    setSigning({ method: 'pk', key: pk });
    await setPrivKey(pk)
    await localStorage.setItem('pk', pk);
  }

  const setSigningMethod = async (meth) => {
    await setSigning({ ...signing, method: meth })
  }

  const signingWallet = () => {
    setSigning({ method: 'sign', key: "" })
  }

  const decryptKey = async (pw) => {
    const singing = await localStorage.getItem('signing');
    const encrypted = signing.key
    const decryptedObj = CryptoJS.RC4Drop.decrypt(encrypted, pw)
    if (decryptedObj.sigBytes < 0) return null
    return decryptedObj.toString(CryptoJS.enc.Utf8)
  }

  const encryptKey = async (pk, pw) => {
    const encrypted = CryptoJS.RC4Drop.encrypt(pk, pw);
    setSigning({ method: 'pk+pw', key: encrypted })
  }

  const logout = () => {
    localStorage.removeItem('acct', null);
    localStorage.removeItem('signing', null);
    window.location.reload();
  };

  const hasWallet = () => {
    if (signing.method === 'sign') return true
    if (signing.method === 'pk') return true
    if (signing.method === 'pk+pw') return true
    return false
  }


var mkReq = function(cmd) {
  return {
    headers: {
      "Content-Type": "application/json"
    },
    method: "POST",
    body: JSON.stringify(cmd)
  };
};

var parseRes = async function (raw) {
  const rawRes = await raw;
  const res = await rawRes;
  if (res.ok){
     const resJSON = await rawRes.json();
     return resJSON;
   } else {
     const resTEXT = await rawRes.text();
     return resTEXT;
   }
};


//------------------------------------------------------------------------------------------------------------------------
//                  END KPENNY FUNCTIONS ONLY
//------------------------------------------------------------------------------------------------------------------------

//COMPUTE_OUT


  return (
    <WalletContext.Provider
      value={{
        GAS_PRICE,
        account,
        setVerifiedAccount,
        privKey,
        storePrivKey,
        getCorrectBalance,
        localRes,
        polling,
        setSigning,
        poolBalance,
        sendRes,
        clearSendRes,
        signing,
        setSigningMethod,
        encryptKey,
        signingWallet,
        walletSuccess,
        setWalletSuccess,
        registered,
        setRegistered,
        logout,
        hasWallet,
        ttl,
        setTtl,
        sigView,
        setSigView,
        pw,
        setPw
      }}
    >
      {props.children}
    </WalletContext.Provider>
  );
};

export const PactConsumer = WalletContext.Consumer;

export const withWalletContext = (Component) => (props) => (
  <PactConsumer>{(providerProps) => <Component {...props} sessionContextProps={providerProps} />}</PactConsumer>
);

//ORIGINAL LISTEN
// const listen = async (reqKey) => {
//   const res = await Pact.fetch.listen({listen: reqKey}, network);
//   setSendRes(res);
//   if (res.result.status === 'success') {
//     notificationContext.showNotification({
//             title: 'Transaction Success!',
//             message: 'Check it out in the block explorer',
//             type: STATUSES.SUCCESS,
//             onClose: async () => {
//               await toast.dismiss(toastId)
//               await window.open(
//                 `https://explorer.chainweb.com/testnet/tx/${res.reqKey}`,
//                 "_blank",
//                 'noopener,noreferrer'
//               );
//               window.location.reload()
//             },
//             onOpen: async (value) => {
//               await toast.dismiss(toastId.current)
//             }
//           }
//     )
//   } else {
//     notificationContext.showNotification({
//             title: 'Transaction Failure!',
//             message: 'Check it out in the block explorer',
//             type: STATUSES.ERROR,
//             onClose: async () => {
//               await toast.dismiss(toastId)
//               await window.open(
//                 `https://explorer.chainweb.com/testnet/tx/${res.reqKey}`,
//                 "_blank",
//                 'noopener,noreferrer'
//               );
//               window.location.reload()
//             },
//             onOpen: async (value) => {
//               await toast.dismiss(toastId.current)
//             }
//           }
//     )
//   }
// }
