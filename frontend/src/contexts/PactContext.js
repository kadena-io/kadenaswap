import React, { useState, createContext, useEffect, useContext, useReducer } from 'react';
import Pact from "pact-lang-api";
import AES from 'crypto-js/aes'
import CryptoJS from 'crypto-js'
import { NotificationContext, STATUSES } from './NotificationContext';
import PasswordPopup from '../components/shared/PasswordPopup';
import { toast } from 'react-toastify';
import pairTokens from '../constants/pairs.json'
import swal from '@sweetalert/with-react'
import pwPrompt from '../components/alerts/pwPrompt'
import walletError from '../components/alerts/walletError'
import walletSigError from '../components/alerts/walletSigError'
import walletLoading from '../components/alerts/walletLoading'
import { reduceBalance, extractDecimal } from '../utils/reduceBalance'
import tokenData from '../constants/cryptoCurrencies';
const fetch = require("node-fetch");

export const PactContext = createContext();
const savedAcct = localStorage.getItem('acct');
const savedPrivKey = localStorage.getItem('pk');
const savedNetwork = localStorage.getItem('network');
const savedSlippage = localStorage.getItem('slippage');
const savedSigning = localStorage.getItem('signing');
const savedTtl = localStorage.getItem('ttl');
const chainId = "1";
const PRECISION = 12;
const NETWORKID = 'mainnet01';
const FEE = 0.003
const network = `https://api.chainweb.com/chainweb/0.0/${NETWORKID}/chain/${chainId}/pact`;

const creationTime = () => Math.round((new Date).getTime()/1000)-10;
const GAS_PRICE = 0.000000000001;

export const PactProvider = (props) => {
  const notificationContext = useContext(NotificationContext);
  const [account, setAccount] = useState((savedAcct ? JSON.parse(savedAcct) : {account: null, guard: null, balance: 0}));
  const [tokenAccount, setTokenAccount] = useState({account: null, guard: null, balance: 0});
  const [privKey, setPrivKey] = useState((savedPrivKey ? savedPrivKey : ""));
  const keyPair = privKey ? Pact.crypto.restoreKeyPairFromSecretKey(privKey) : "";
  const [tokenFromAccount, setTokenFromAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenToAccount, setTokenToAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenList, setTokenList] = useState(tokenData);
  const [precision, setPrecision] = useState(false);
  const [pairAccount, setPairAccount] = useState("");
  const [pairReserve, setPairReserve] = useState("");
  const [pair, setPair] = useState("");
  const [ratio, setRatio] = useState(NaN);
  const [pairAccountBalance, setPairAccountBalance] = useState(null);
  const [supplied, setSupplied] = useState(false);
  const [slippage, setSlippage] = useState((savedSlippage ? savedSlippage : 0.05));
  const [liquidityProviderFee, setLiquidityProviderFee] = useState(0.003);
  const [cmd, setCmd] = useState(null);
  const [localRes, setLocalRes] = useState(null);
  const [polling, setPolling] = useState(false);
  const [totalSupply, setTotalSupply] = useState("")
  const [pairList, setPairList] = useState(pairTokens)
  const [pairListAccount, setPairListAccount] = useState(pairTokens)
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
    pairReserve ? setRatio(pairReserve['token0']/pairReserve['token1']) : setRatio(NaN)
  }, [pairReserve]);

  useEffect(() => {
    if (account.account) setVerifiedAccount(account.account);
  }, [sendRes])

  useEffect(() => {
    const store = async () => localStorage.setItem('signing', JSON.stringify(signing));
    store()
  }, [signing])

  useEffect(() => {
    fetchPrecision();
  }, [precision])

  useEffect(() => {
    fetchAllBalances();
  }, [balances, account.account, sendRes])

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

  const storeSlippage = async (slippage) => {
    await setSlippage(slippage)
    await localStorage.setItem('slippage', slippage);
  }

  const storeTtl = async (ttl) => {
    await setTtl(slippage)
    await localStorage.setItem('ttl', ttl);
  }

  const fetchAllBalances = async () => {
    let count=0;
    let endBracket = ''
    let tokenNames = Object.values(tokenData).reduce((accum, cumul)=> {
      count++;
      endBracket+=')'
      let code =  `
      (let
        ((${cumul.name}
          (try -1 (${cumul.code}.get-balance "${account.account}"))
      ))`
      accum+=code;
      return accum;
    }, '')
    let objFormat =  `{${Object.keys(tokenData).map(token => `"${token}": ${token}`).join(',')}}`
    tokenNames = tokenNames + objFormat + endBracket;
    try {
      let data = await Pact.fetch.local({
          pactCode: tokenNames,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
      if (data.result.status === "success"){
        Object.keys(tokenData).forEach(token => {
          tokenData[token].balance = extractDecimal(data.result.data[token])===-1
            ? '0'
            : extractDecimal(data.result.data[token]);
        })
        setBalances(true)
      } else {
        setBalances(false)
      }
    } catch (e) {
      console.log(e)
      setBalances(true);
    }
  }

  const fetchPrecision = async () => {
    let count=0;
    let endBracket = ''
    let tokenNames = Object.values(tokenData).reduce((accum, cumul)=> {
      count++;
      endBracket+=')'
      let code =  `
      (let
        ((${cumul.name}
          (try -1 (${cumul.code}.precision))
      ))`
      accum+=code;
      return accum;
    }, '')
    let objFormat =  `{${Object.keys(tokenData).map(token => `"${token}": ${token}`).join(',')}}`
    tokenNames = tokenNames + objFormat + endBracket;
    try {
      let data = await Pact.fetch.local({
          pactCode: tokenNames,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
      if (data.result.status === "success"){
        Object.keys(tokenData).forEach(token => {
          tokenData[token].precision = extractDecimal(data.result.data[token]);
        })
        setPrecision(true);
      }
    } catch (e) {
      setPrecision(false);

      console.log(e)
    }
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

  const getTokenAccount = async (token, account, first) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(${token}.details ${JSON.stringify(account)})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,0.01,100000000, 28800, creationTime()),
        }, network);
        if (data.result.status === "success"){
          // setTokenAccount({...data.result.data, balance: getCorrectBalance(data.result.data.balance)});
          first ? setTokenFromAccount(data.result.data) : setTokenToAccount(data.result.data)
          return data.result.data
        } else if (data.result.status === "failure"){
          first ? setTokenFromAccount({ account: null, guard: null, balance: 0 }) : setTokenToAccount({ account: null, guard: null, balance: 0 })
          return { account: null, guard: null, balance: 0 }
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getTotalTokenSupply = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.tokens.total-supply (kswap.exchange.get-pair-key ${token0} ${token1}))`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,0.01,100000000, 28800, creationTime()),
        }, network);
        if (data.result.status === "success"){
          if (data.result.data.decimal) setTotalSupply(data.result.data.decimal);
          else setTotalSupply(data.result.data);
        }
    } catch (e) {
      console.log(e)
    }
  }

  const createTokenPairLocal = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.create-pair
              ${token0.code}
              ${token1.code}
              ""
            )`,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,5000,creationTime(),28800),
          networkId: NETWORKID
      }, network);
      let pair =  data.result.data.account
      try {
        let cmd = {
            pactCode: `
            (kswap.exchange.create-pair
                ${token0.code}
                ${token1.code}
                ""
            )
            (kswap.exchange.add-liquidity
                ${token0.code}
                ${token1.code}
                (read-decimal 'amountDesired0)
                (read-decimal 'amountDesired1)
                (read-decimal 'amountMinimum0)
                (read-decimal 'amountMinimum1)
                ${JSON.stringify(account.account)}
                ${JSON.stringify(account.account)}
                (read-keyset 'user-ks)
              )`,
            keyPairs: {
              ...keyPair,
              clist: [
                {name: `${token0.code}.TRANSFER`, args: [account.account, pair, Number(amountDesired0)]},
                {name: `${token1.code}.TRANSFER`, args: [account.account, pair, Number(amountDesired1)]},
                {name: "kswap.gas-station.GAS_PAYER", args: ["free-gas", {int: 1}, 1.0]},
              ]
            },
            envData: {
              "user-ks": [keyPair.publicKey],
              "amountDesired0": reduceBalance(amountDesired0,tokenData[token0.name].precision),
              "amountDesired1": reduceBalance(amountDesired1,tokenData[token1.name].precision),
              "amountMinimum0": reduceBalance(amountDesired0*(1-parseFloat(slippage)),tokenData[token0.name].precision),
              "amountMinimum1": reduceBalance(amountDesired1*(1-parseFloat(slippage)),tokenData[token1.name].precision)
            },
            meta: Pact.lang.mkMeta("kswap-free-gas", chainId ,GAS_PRICE,5000,creationTime(), 600),
            networkId: NETWORKID
          };
          let data = await Pact.fetch.local(cmd, network);
          setCmd(cmd);
          setLocalRes(data);
          return data;
        } catch (e) {
          setLocalRes({});
          console.log(e)
          return -1
        }
    } catch (e) {
      console.log(e)
    }
  }

  const addLiquidityLocal = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = await pwPrompt();
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return
      }
      let pair = await getPairAccount(token0.code, token1.code);
      let cmd = {
          pactCode: `(kswap.exchange.add-liquidity
              ${token0.code}
              ${token1.code}
              (read-decimal 'amountDesired0)
              (read-decimal 'amountDesired1)
              (read-decimal 'amountMinimum0)
              (read-decimal 'amountMinimum1)
              ${JSON.stringify(account.account)}
              ${JSON.stringify(account.account)}
              (read-keyset 'user-ks)
            )`,
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: `${token0.code}.TRANSFER`, args: [account.account, pair, Number(amountDesired0)]},
              {name: `${token1.code}.TRANSFER`, args: [account.account, pair, Number(amountDesired1)]},
              {name: "kswap.gas-station.GAS_PAYER", args: ["free-gas", {int: 1}, 1.0]},
            ]
          },
          envData: {
            "user-ks": account.guard,
            "amountDesired0": reduceBalance(amountDesired0,tokenData[token0.name].precision),
            "amountDesired1": reduceBalance(amountDesired1,tokenData[token1.name].precision),
            "amountMinimum0": reduceBalance(amountDesired0*(1-parseFloat(slippage)),tokenData[token0.name].precision),
            "amountMinimum1": reduceBalance(amountDesired1*(1-parseFloat(slippage)),tokenData[token1.name].precision)
          },
          meta: Pact.lang.mkMeta("kswap-free-gas", chainId ,GAS_PRICE,3000,creationTime(), 600),
          networkId: NETWORKID
        };
      let data = await Pact.fetch.local(cmd, network);
      setCmd(cmd);
      setLocalRes(data);
      return data;
    } catch (e) {
      setLocalRes({});
      console.log(e)
      return -1
    }
  }

  const addLiquidityWallet = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let pair = await getPairAccount(token0.code, token1.code);
      const signCmd = {
        pactCode: `(kswap.exchange.add-liquidity
            ${token0.code}
            ${token1.code}
            (read-decimal 'amountDesired0)
            (read-decimal 'amountDesired1)
            (read-decimal 'amountMinimum0)
            (read-decimal 'amountMinimum1)
            ${JSON.stringify(account.account)}
            ${JSON.stringify(account.account)}
            (read-keyset 'user-ks)
          )`,
        caps: [
          Pact.lang.mkCap("Gas Station", "free gas", "kswap.gas-station.GAS_PAYER", ["free-gas", {int: 1}, 1.0]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `${token0.code}.TRANSFER`, [account.account, pair, Number(amountDesired0)]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `${token1.code}.TRANSFER`, [account.account, pair, Number(amountDesired1)]),
        ],
        sender: "kswap-free-gas",
        gasLimit: 3000,
        gasPrice: GAS_PRICE,
        chainId: chainId,
        ttl: 600,
        envData: {
          "user-ks": account.guard,
          "amountDesired0": reduceBalance(amountDesired0,tokenData[token0.name].precision),
          "amountDesired1": reduceBalance(amountDesired1,tokenData[token1.name].precision),
          "amountMinimum0": reduceBalance(amountDesired0*(1-parseFloat(slippage)),tokenData[token0.name].precision),
          "amountMinimum1": reduceBalance(amountDesired1*(1-parseFloat(slippage)),tokenData[token1.name].precision)
        },
        signingPubKey: account.guard.keys[0],
        networkId: NETWORKID,
      }
      //alert to sign tx
      walletLoading();
      const cmd = await Pact.wallet.sign(signCmd);
      //close alert programmatically
      swal.close()
      setWalletSuccess(true)
      //set signedtx
      setCmd(cmd);
      let data = await fetch(`${network}/api/v1/local`, mkReq(cmd))
      data = await parseRes(data);
      setLocalRes(data);
      return data;
    } catch (e) {
      //wallet error alert
      if (e.message.includes('Failed to fetch')) walletError()
      else walletSigError()
      console.log(e)
    }
  }

  const removeLiquidityLocal = async (token0, token1, liquidity) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = await pwPrompt();
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return
      }
      let pairKey = await getPairKey(token0, token1);
      let pair = await getPairAccount(token0, token1);
      let cmd = {
          pactCode: `(kswap.exchange.remove-liquidity
              ${token0}
              ${token1}
              (read-decimal 'liquidity)
              0.0
              0.0
              ${JSON.stringify(account.account)}
              ${JSON.stringify(account.account)}
              (read-keyset 'user-ks)
            )`,
            networkId: NETWORKID,
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: `kswap.tokens.TRANSFER`, args: [pairKey, account.account, pair, Number(liquidity)]},
              {name: `kswap.tokens.TRANSFER`, args: [pairKey, account.account, pair, Number(liquidity)]},
              {name: "kswap.gas-station.GAS_PAYER", args: ["free-gas", {int: 1}, 1.0]},
            ]
          },
          envData: {
            "user-ks": account.guard,
            "liquidity": reduceBalance(liquidity,PRECISION),
          },
          meta: Pact.lang.mkMeta("kswap-free-gas", chainId ,GAS_PRICE,3000,creationTime(), 600),
        };
        setCmd(cmd);
        let data = await Pact.fetch.local(cmd, network);
        setLocalRes(data);
        return data;
      } catch (e) {
        setLocalRes({});
        if (e.message.includes('Failed to fetch')) walletError()
        else walletSigError();
        return -1;
      }
  }

  const removeLiquidityWallet = async (token0, token1, liquidity) => {
    try {
      let pairKey = await getPairKey(token0, token1);
      let pair = await getPairAccount(token0, token1);
      const signCmd = {
        pactCode:`(kswap.exchange.remove-liquidity
            ${token0}
            ${token1}
            (read-decimal 'liquidity)
            0.0
            0.0
            ${JSON.stringify(account.account)}
            ${JSON.stringify(account.account)}
            (read-keyset 'user-ks)
          )`,
        caps: [
          Pact.lang.mkCap("Gas Station", "free gas", "kswap.gas-station.GAS_PAYER", ["free-gas", {int: 1}, 1.0]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `kswap.tokens.TRANSFER`, [pairKey, account.account, pair, Number(liquidity)]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `kswap.tokens.TRANSFER`, [pairKey, account.account, pair, Number(liquidity)]),
        ],
        sender: "kswap-free-gas",
        gasLimit: 3000,
        gasPrice: GAS_PRICE,
        chainId: chainId,
        ttl: 600,
        envData: {
          "user-ks": account.guard,
          "liquidity": reduceBalance(liquidity,PRECISION)
        },
        signingPubKey: account.guard.keys[0],
        networkId: NETWORKID,
      }
      //alert to sign tx
      walletLoading();
      const cmd = await Pact.wallet.sign(signCmd);
      //close alert programmatically
      swal.close()
      setWalletSuccess(true)
      setCmd(cmd);
      let data = await fetch(`${network}/api/v1/local`, mkReq(cmd))
      data = await parseRes(data);
      setLocalRes(data);
      return data;
    } catch (e) {
      //wallet error alert
      setLocalRes({});
      if (e.message.includes('Failed to fetch')) walletError()
      else walletSigError()
      console.log(e)
    }
  }

  const getPairAccount = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(at 'account (kswap.exchange.get-pair ${token0} ${token1}))`,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          setPairAccount(data.result.data);
          return data.result.data;
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPair = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.get-pair ${token0} ${token1})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          setPair(data.result.data);
          return data.result.data;
        } else {
          return null;
        }
    } catch (e) {
      console.log(e)
    }
  }


  const getPairKey = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.get-pair-key ${token0} ${token1})`,
          meta: Pact.lang.mkMeta(account.account, chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          return data.result.data;
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPairAccountBalance = async (token0, token1, account) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.tokens.get-balance (kswap.exchange.get-pair-key ${token0} ${token1}) ${JSON.stringify(account)})`,
          meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          setPairAccountBalance(data.result.data);
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPairListAccountBalance = async (account) => {
      try {
        const tokenPairList = Object.keys(pairList).reduce((accum, pair) => {
          accum+=`[${ pair.split(":").join(" ")}] `
          return accum
        }, "")
        let data = await Pact.fetch.local({
            pactCode: `
            (namespace 'free)

            (module kswap-read G

              (defcap G ()
                true)

              (defun pair-info (pairList:list)
                (let* (
                  (token0 (at 0 pairList))
                  (token1 (at 1 pairList))
                  (p (kswap.exchange.get-pair token0 token1))
                  (reserveA (kswap.exchange.reserve-for p token0))
                  (reserveB (kswap.exchange.reserve-for p token1))
                  (totalBal (kswap.tokens.total-supply (kswap.exchange.get-pair-key token0 token1)))
                  (acctBal
                      (try 0.0 (kswap.tokens.get-balance (kswap.exchange.get-pair-key token0 token1) ${JSON.stringify(account)})
                    ))
                )
                [(kswap.exchange.get-pair-key token0 token1)
                 reserveA
                 reserveB
                 totalBal
                 acctBal
                 (* reserveA (/ acctBal totalBal))
                 (* reserveB (/ acctBal totalBal))
               ]
              ))
            )
            (map (kswap-read.pair-info) [${tokenPairList}])
             `,
            meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
          }, network);
        if (data.result.status === "success"){
          let dataList = data.result.data.reduce((accum, data) => {
            accum[data[0]] = {
              balance: data[4],
              supply: data[3],
              reserves:[data[1], data[2]],
              pooledAmount: [data[5], data[6]]
            };
            return accum;
          }, {})
          const pairList = Object.values(pairTokens).map(pair => {
            return {
              ...pair,
              ...dataList[pair.name]
             }
          })
          setPairListAccount(pairList);
        }
      } catch (e) {
        console.log(e)
      }
  }

  const getPairList = async () => {
      try {
        const tokenPairList = Object.keys(pairList).reduce((accum, pair) => {
          accum+=`[${ pair.split(":").join(" ")}] `
          return accum
        }, "")
        let data = await Pact.fetch.local({
            pactCode: `
            (namespace 'free)

            (module kswap-read G

              (defcap G ()
                true)

              (defun pair-info (pairList:list)
                (let* (
                  (token0 (at 0 pairList))
                  (token1 (at 1 pairList))
                  (p (kswap.exchange.get-pair token0 token1))
                  (reserveA (kswap.exchange.reserve-for p token0))
                  (reserveB (kswap.exchange.reserve-for p token1))
                  (totalBal (kswap.tokens.total-supply (kswap.exchange.get-pair-key token0 token1)))
                )
                [(kswap.exchange.get-pair-key token0 token1)
                 reserveA
                 reserveB
                 totalBal
               ]
              ))
            )
            (map (kswap-read.pair-info) [${tokenPairList}])
             `,
            meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
          }, network);
        if (data.result.status === "success"){
          let dataList = data.result.data.reduce((accum, data) => {
            accum[data[0]] = {
              supply: data[3],
              reserves:[data[1], data[2]]
            };
            return accum;
          }, {})
          const pairList = Object.values(pairTokens).map(pair => {
            return {
              ...pair,
              ...dataList[pair.name]
             }
          })
          setPairList(pairList);
        }
      } catch (e) {
        console.log(e)
      }
  }


  const getReserves = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `
          (use kswap.exchange)
          (let*
            (
              (p (get-pair ${token0} ${token1}))
              (reserveA (reserve-for p ${token0}))
              (reserveB (reserve-for p ${token1}))
            )[reserveA reserveB])
           `,
           meta: Pact.lang.mkMeta("account", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          await setPairReserve({token0: data.result.data[0].decimal? data.result.data[0].decimal:  data.result.data[0], token1: data.result.data[1].decimal? data.result.data[1].decimal:  data.result.data[1]});
        } else {
          await setPairReserve({});
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPooledAmount = async (pairKey, token0, token1, account) => {
    let pair = await getPairAccount(token0, token1);
    try {
      let data = await Pact.fetch.local({
          pactCode: `
          (use kswap.exchange)
          (let*
            (
              (p (get-pair ${token0} ${token1}))
              (reserveA (reserve-for p ${token0}))
              (reserveB (reserve-for p ${token1}))
              (totalBal (kswap.tokens.total-supply (kswap.exchange.get-pair-key ${token0} ${token1})))
              (acctBal (kswap.tokens.get-balance (kswap.exchange.get-pair-key ${token0} ${token1}) ${JSON.stringify(account)}))
            )[(* reserveA (/ acctBal totalBal))(* reserveB (/ acctBal totalBal))])
           `,
           meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        let balance0= data.result.data[0].decimal?data.result.data[0].decimal :data.result.data[0] ;
        let balance1= data.result.data[1].decimal?data.result.data[1].decimal :data.result.data[1] ;
        setPoolBalance([balance0, balance1]);
    } catch (e) {
      console.log(e)
    }
  }

  const tokens = async (token0, token1, account) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `
          (kswap.tokens.get-tokens)
           `,
           meta: Pact.lang.mkMeta("", chainId ,GAS_PRICE,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          return data.result.data;
        } else {
          await setPairReserve(null)
          console.log("Failed")
        }
    } catch (e) {
      console.log(e)
    }
  }


  const swap = async (token0, token1, isSwapIn) => {
    try {
      let pair = await getPairAccount(token0.address, token1.address);

      const inPactCode = `(kswap.exchange.swap-exact-in
          (read-decimal 'token0Amount)
          (read-decimal 'token1AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          (read-decimal 'token1Amount)
          (read-decimal 'token0AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const cmd = {
          pactCode: (isSwapIn ? inPactCode : outPactCode),
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              { name: `${token0.address}.TRANSFER`,
                args: [
                  account.account,
                  pair,
                  isSwapIn
                    ? reduceBalance(token0.amount, tokenData[token0.coin].precision)
                    : reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision)
                ]
              },
            ]
          },
          envData: {
            "user-ks": account.guard,
            "token0Amount": reduceBalance(token0.amount, tokenData[token0.coin].precision),
            "token1Amount": reduceBalance(token1.amount, tokenData[token1.coin].precision),
            "token1AmountWithSlippage": reduceBalance(token1.amount*(1-parseFloat(slippage)), tokenData[token1.coin].precision),
            "token0AmountWithSlippage": reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision)
          },
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
          networkId: NETWORKID,
          meta: Pact.lang.mkMeta(account.account, chainId ,GAS_PRICE, 3000, creationTime(), 600),
      }
      setCmd(cmd);
      let data = await Pact.fetch.send(cmd, network);
    } catch (e) {
      console.log(e)
    }
  }

  const swapLocal = async (token0, token1, isSwapIn) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = await pwPrompt();
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return -1
      }
      const ct = creationTime();
      let pair = await getPairAccount(token0.address, token1.address);
      const inPactCode = `(kswap.exchange.swap-exact-in
          (read-decimal 'token0Amount)
          (read-decimal 'token1AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          (read-decimal 'token1Amount)
          (read-decimal 'token0AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const cmd = {
          pactCode: (isSwapIn ? inPactCode : outPactCode),
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              { name:
                "kswap.gas-station.GAS_PAYER",
                args: ["free-gas", {int: 1}, 1.0]
              },
              { name:
                `${token0.address}.TRANSFER`,
                args: [
                  account.account,
                  pair,
                  isSwapIn
                    ? reduceBalance(token0.amount, tokenData[token0.coin].precision)
                    : reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision)
                ]
              },
            ]
          },
          envData: {
            "user-ks": account.guard,
            "token0Amount": reduceBalance(token0.amount, tokenData[token0.coin].precision),
            "token1Amount": reduceBalance(token1.amount, tokenData[token1.coin].precision),
            "token1AmountWithSlippage": reduceBalance(token1.amount*(1-parseFloat(slippage)), tokenData[token1.coin].precision),
            "token0AmountWithSlippage": reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision)
          },
          networkId: NETWORKID,
          meta: Pact.lang.mkMeta("kswap-free-gas", chainId, GAS_PRICE, 3000, ct, 600),
      }
      setCmd(cmd);
      let data = await Pact.fetch.local(cmd, network);
      setLocalRes(data);
      return data;
    } catch (e) {
      console.log(e)
      setLocalRes({});
      return -1
    }
  }

  const swapWallet = async (token0, token1, isSwapIn) => {
    try {
      const inPactCode = `(kswap.exchange.swap-exact-in
          (read-decimal 'token0Amount)
          (read-decimal 'token1AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          (read-decimal 'token1Amount)
          (read-decimal 'token0AmountWithSlippage)
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
        )`
      const signCmd = {
        pactCode: (isSwapIn ? inPactCode : outPactCode),
        caps: [
          Pact.lang.mkCap(
            "Gas Station",
            "free gas",
            "kswap.gas-station.GAS_PAYER",
            ["free-gas", {int: 1}, 1.0]
          ),
          Pact.lang.mkCap(
            "transfer capability",
            "trasnsfer token in",
            `${token0.address}.TRANSFER`,
            [
              account.account,
              pair.account,
              isSwapIn
                ? reduceBalance(token0.amount, tokenData[token0.coin].precision)
                : reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision)
            ]
          ),
        ],
        sender: "kswap-free-gas",
        gasLimit: 3000,
        gasPrice: GAS_PRICE,
        chainId: chainId,
        ttl: 600,
        envData: {
          "user-ks": account.guard,
          "token0Amount": reduceBalance(token0.amount, tokenData[token0.coin].precision),
          "token1Amount": reduceBalance(token1.amount, tokenData[token1.coin].precision),
          "token0AmountWithSlippage": reduceBalance(token0.amount*(1+parseFloat(slippage)), tokenData[token0.coin].precision),
          "token1AmountWithSlippage": reduceBalance(token1.amount*(1-parseFloat(slippage)), tokenData[token1.coin].precision)
        },
        signingPubKey: account.guard.keys[0],
        networkId: NETWORKID,
      }
      //alert to sign tx
      walletLoading();
      const cmd = await Pact.wallet.sign(signCmd);
      //close alert programmatically
      swal.close()
      setWalletSuccess(true)
      //set signedtx
      setCmd(cmd);
      let data = await fetch(`${network}/api/v1/local`, mkReq(cmd))
      data = await parseRes(data);
      setLocalRes(data);
      return data;
    } catch (e) {
      //wallet error alert
      setLocalRes({});
      if (e.message.includes('Failed to fetch')) walletError()
      else walletSigError()
      console.log(e)
    }

  }

  const swapSend = async () => {
    setPolling(true)
    try {
      let data
      if (cmd.pactCode){
        data = await Pact.fetch.send(cmd, network)
      } else {
        data = await Pact.wallet.sendSigned(cmd, network)
      }
      pollingNotif(data.requestKeys[0]);
      await listen(data.requestKeys[0]);
      setPolling(false)
    } catch (e) {
      setPolling(false)
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

  const getRatio = (toToken, fromToken) => {
    if (toToken===fromToken) return 1;
    return pairReserve["token1"]/pairReserve["token0"]
  }

  const getRatio1 = (toToken, fromToken) => {
    if (toToken===fromToken) return 1;
    return pairReserve["token0"]/pairReserve["token1"]
  }

  const share = (amount) => {
    return Number(amount)/(Number(pairReserve["token0"])+Number(amount));
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


//------------------------------------------------------------------------------------------------------------------------
//                  START KPENNY FUNCTIONS ONLY
//------------------------------------------------------------------------------------------------------------------------

const kpennyReserveLocal = async (amtKda) => {
  try {
    let privKey = signing.key
    if (signing.method === 'pk+pw') {
      const pw = await pwPrompt();
      privKey = await decryptKey(pw)
    }
    if (privKey.length !== 64) {
      return -1
    }
    const ct = creationTime();
    const pactCode = `(kswap.kpenny.reserve ${JSON.stringify(account.account)} (read-decimal 'amt))`
    const cmd = {
        pactCode: pactCode,
        keyPairs: {
          publicKey: account.guard.keys[0],
          secretKey: privKey,
          clist: [
            { name:
              "kswap.gas-station.GAS_PAYER",
              args: ["free-gas", {int: 1}, 1.0]
            },
            { name:
              `coin.TRANSFER`,
              args: [
                account.account,
                "kpenny-bank",
                reduceBalance(amtKda, 12),
              ]
            },
          ]
        },
        envData: {
          "amt": amtKda
        },
        networkId: NETWORKID,
        meta: Pact.lang.mkMeta("kswap-free-gas", chainId, GAS_PRICE, 3000, ct, 600),
    }
    setCmd(cmd);
    let data = await Pact.fetch.local(cmd, network);
    setLocalRes(data);
    return data;
  } catch (e) {
    console.log(e)
    setLocalRes({});
    return -1
  }
}

const kpennyReserveWallet = async (amtKda) => {
  try {
    const pactCode = `(kswap.kpenny.reserve ${JSON.stringify(account.account)} (read-decimal 'amt))`
    const signCmd = {
      pactCode: pactCode,
      caps: [
        Pact.lang.mkCap(
          "Gas Station",
          "free gas",
          "kswap.gas-station.GAS_PAYER",
          ["free-gas", {int: 1}, 1.0]
        ),
        Pact.lang.mkCap(
          "transfer capability",
          "trasnsfer token in",
          `coin.TRANSFER`,
          [
            account.account,
            "kpenny-bank",
            reduceBalance(amtKda, 12)
          ]
        ),
      ],
      sender: "kswap-free-gas",
      gasLimit: 3000,
      gasPrice: GAS_PRICE,
      chainId: chainId,
      ttl: 600,
      envData: {
        "amt": amtKda
      },
      signingPubKey: account.guard.keys[0],
      networkId: NETWORKID,

    }
    //alert to sign tx
    walletLoading();
    console.log(signCmd)
    const cmd = await Pact.wallet.sign(signCmd);
    //close alert programmatically
    swal.close()
    setWalletSuccess(true)
    const res = await Pact.wallet.sendSigned(cmd, network);
    console.log(res)
    //this is a small hack to get the polling header widget to work
    setLocalRes({ reqKey: res.requestKeys[0] })
    setPolling(true)
    pollingNotif(res.requestKeys[0]);
    await listen(res.requestKeys[0]);
    setPolling(false)
  } catch (e) {
    //wallet error alert
    if (e.message.includes('Failed to fetch')) walletError()
    else walletSigError()
    console.log(e)
  }
}

const kpennyRedeemLocal = async () => {
  try {
    let privKey = signing.key
    if (signing.method === 'pk+pw') {
      const pw = await pwPrompt();
      privKey = await decryptKey(pw)
    }
    if (privKey.length !== 64) {
      return -1
    }
    const ct = creationTime();
    const pactCode = `(kswap.kpenny.redeem ${JSON.stringify(account.account)} ${JSON.stringify(account.account)} (read-keyset "user-ks"))`
    const cmd = {
        pactCode: pactCode,
        keyPairs: {
          publicKey: account.guard.keys[0],
          secretKey: privKey,
          clist: [
            // { name:
            //   "kswap.gas-station.GAS_PAYER",
            //   args: ["free-gas", {int: 1}, 1.0]
            // },
            // { name:
            //   `coin.TRANSFER`,
            //   args: [
            //     account.account,
            //     "kpenny-bank",
            //     reduceBalance(amtKda, 12),
            //   ]
            // },
          ]
        },
        envData: {
          "user-ks": account.guard,
        },
        networkId: NETWORKID,
        meta: Pact.lang.mkMeta(account.account, chainId, GAS_PRICE, 3000, ct, 600),
    }
    console.log(cmd)
    setCmd(cmd);
    let data = await Pact.fetch.local(cmd, network);
    console.log(data)
    setLocalRes(data);
    return data;
  } catch (e) {
    console.log(e)
    setLocalRes({});
    return -1
  }
}

const kpennyRedeemWallet = async () => {
  try {
    const pactCode = `(kswap.kpenny.redeem ${JSON.stringify(account.account)} ${JSON.stringify(account.account)} (read-keyset "user-ks"))`
    const signCmd = {
      pactCode: pactCode,
      caps: [],
      sender: account.account,
      gasLimit: 3000,
      gasPrice: GAS_PRICE,
      chainId: chainId,
      ttl: 600,
      envData: {
        "user-ks": account.guard,
      },
      signingPubKey: account.guard.keys[0],
      networkId: NETWORKID,

    }
    //alert to sign tx
    walletLoading();
    console.log(signCmd)
    const cmd = await Pact.wallet.sign(signCmd);
    //close alert programmatically
    swal.close()
    setWalletSuccess(true)
    const res = await Pact.wallet.sendSigned(cmd, network);
    console.log(res)
    //this is a small hack to get the polling header widget to work
    setLocalRes({ reqKey: res.requestKeys[0] })
    setPolling(true)
    pollingNotif(res.requestKeys[0]);
    await listen(res.requestKeys[0]);
    setPolling(false)
  } catch (e) {
    //wallet error alert
    if (e.message.includes('Failed to fetch')) walletError()
    else walletSigError()
    console.log(e)
  }
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

var computeOut = function (amountIn) {
  let reserveOut = Number(pairReserve['token1']);
  let reserveIn = Number(pairReserve['token0']);
  let numerator = Number((amountIn * (1-FEE)) * reserveOut);
  let denominator = Number(reserveIn + (amountIn * (1-FEE)))
  return numerator / denominator;
};

//COMPUTE_IN
var computeIn = function (amountOut) {
  let reserveOut = Number(pairReserve['token1']);
  let reserveIn = Number(pairReserve['token0']);
  let numerator = Number(reserveIn * amountOut)
  let denominator = Number((reserveOut-amountOut) *(1-FEE))
  // round up the last digit
  return numerator / denominator;
};

function computePriceImpact(amountIn, amountOut) {
  const reserveOut = Number(pairReserve['token1']);
  const reserveIn = Number(pairReserve['token0']);
  const midPrice = (reserveOut/reserveIn);
  const exactQuote = amountIn * midPrice;
  const slippage = (exactQuote-amountOut) / exactQuote;
  return slippage;
}

function priceImpactWithoutFee(priceImpact){
  return priceImpact - realizedLPFee();
}

function realizedLPFee(numHops=1) {
  return 1-((1-FEE)*numHops);
}

  return (
    <PactContext.Provider
      value={{
        GAS_PRICE,
        PRECISION,
        tokens,
        pairList,
        account,
        setVerifiedAccount,
        getTokenAccount,
        getRatio,
        getRatio1,
        supplied,
        setSupplied,
        addLiquidityWallet,
        addLiquidityLocal,
        removeLiquidityWallet,
        removeLiquidityLocal,
        createTokenPairLocal,
        pairAccount,
        pairAccountBalance,
        getPairAccount,
        getPairAccountBalance,
        privKey,
        storePrivKey,
        tokenAccount,
        tokenFromAccount,
        tokenToAccount,
        getPair,
        getReserves,
        pairReserve,
        ratio,
        swap,
        swapLocal,
        swapSend,
        slippage,
        storeSlippage,
        getCorrectBalance,
        liquidityProviderFee,
        localRes,
        polling,
        setSigning,
        getPooledAmount,
        getTotalTokenSupply,
        totalSupply,
        share,
        poolBalance,
        pair,
        sendRes,
        clearSendRes,
        signing,
        setSigningMethod,
        encryptKey,
        signingWallet,
        swapWallet,
        walletSuccess,
        setWalletSuccess,
        registered,
        setRegistered,
        logout,
        hasWallet,
        ttl,
        setTtl,
        getPairListAccountBalance,
        getPairList,
        pairListAccount,
        sigView,
        setSigView,
        pw,
        setPw,
        storeTtl,
        tokenData,
        kpennyReserveLocal,
        kpennyReserveWallet,
        kpennyRedeemWallet,
        kpennyRedeemLocal,
        computeIn,
        computeOut,
        computePriceImpact,
        priceImpactWithoutFee
      }}
    >
      {props.children}
    </PactContext.Provider>
  );
};

export const PactConsumer = PactContext.Consumer;

export const withPactContext = (Component) => (props) => (
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
