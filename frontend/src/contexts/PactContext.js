import React, { useState, createContext, useEffect, useContext } from 'react';
import Pact from "pact-lang-api";
import AES from 'crypto-js/aes'
import CryptoJS from 'crypto-js'
import { NotificationContext, STATUSES } from './NotificationContext';
import { toast } from 'react-toastify';
import pairTokens from '../constants/pairTokens'

const keepDecimal = decimal => {
  decimal = parseFloat(decimal).toPrecision(13)
  const num = decimal.toString().indexOf('.') === -1 ? `${decimal}.0` : decimal
  return num
}

export const PactContext = createContext();

const savedAcct = localStorage.getItem('acct');
const savedPrivKey = localStorage.getItem('pk');
const savedNetwork = localStorage.getItem('network');
const savedSlippage = localStorage.getItem('slippage');
const savedSigning = localStorage.getItem('signing');

const network = "https://us1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact";

const chainId = "0";
const creationTime = () => Math.round((new Date).getTime()/1000)-10;

export const PactProvider = (props) => {

  const notificationContext = useContext(NotificationContext);

  const [account, setAccount] = useState((savedAcct ? JSON.parse(savedAcct) : {account: null, guard: null, balance: 0}));
  const [tokenAccount, setTokenAccount] = useState({account: null, guard: null, balance: 0});
  const [privKey, setPrivKey] = useState((savedPrivKey ? savedPrivKey : ""));
  const keyPair = privKey ? Pact.crypto.restoreKeyPairFromSecretKey(privKey) : "";
  const [tokenFromAccount, setTokenFromAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenToAccount, setTokenToAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenList, setTokenList] = useState({tokens: []});
  const [pairAccount, setPairAccount] = useState("");
  const [pairReserve, setPairReserve] = useState("");
  const [pair, setPair] = useState("");
  const [ratio, setRatio] = useState(NaN);
  const [pairAccountBalance, setPairAccountBalance] = useState(null);
  const [supplied, setSupplied] = useState(false);
  const [slippage, setSlippage] = useState((savedSlippage ? savedSlippage : 0.50));
  const [liquidityProviderFee, setLiquidityProviderFee] = useState(0.003);
  const [cmd, setCmd] = useState(null);
  const [localRes, setLocalRes] = useState(null);
  const [polling, setPolling] = useState(false);
  const [totalSupply, setTotalSupply] = useState("")
  const [pairList, setPairList] = useState(pairTokens)
  const [pairListAccount, setPairListAccount] = useState(pairTokens)
  const [poolBalance, setPoolBalance] = useState(["N/A", "N/A"]);
  const [sendRes, setSendRes] = useState(null);
  const [signing, setSigning] = useState(savedSigning ? JSON.parse(savedSigning) : { method: 'none', key: "" })
  const [walletSuccess, setWalletSuccess] = useState(false)
  const [registered, setRegistered] = useState(false);
  const [ttl, setTtl] = useState(2800)
  // const [toastId, setToastId] = useState(null);
  const toastId = React.useRef(null)

  useEffect(() => {
    if (account.account) setRegistered(true);
  }, [registered]);

  useEffect(() => {
    pairReserve ? setRatio(pairReserve['token0']/pairReserve['token1']) : setRatio(NaN)
  }, [pairReserve]);

  useEffect(() => {
    if (account.account) setVerifiedAccount(account.account)
  }, [])

  useEffect(() => {
    const store = async () => localStorage.setItem('signing', JSON.stringify(signing));
    store()
  }, [signing])


  const getCorrectBalance = (balance) => {
    const balanceClean = (!isNaN(balance) ? balance : balance.decimal)
    return balanceClean
  }

  const storeSlippage = async (slippage) => {
    await setSlippage(slippage)
    await localStorage.setItem('slippage', slippage);
  }

  const setVerifiedAccount = async (accountName) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(coin.details ${JSON.stringify(accountName)})`,
          meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        console.log(data)
        if (data.result.status === "success"){
          await localStorage.setItem('acct', JSON.stringify(data.result.data));
          setAccount({...data.result.data, balance: getCorrectBalance(data.result.data.balance)});
          await localStorage.setItem('acct', JSON.stringify(data.result.data));
          console.log("Account is set to ", accountName);
        } else {
          setAccount({account: null, guard: null, balance: 0});
          console.log("Account is not verified")
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getTokenAccount = async (token, account, first) => {
    console.log("gettokenaccount", token, `(${token}.details ${JSON.stringify(account)})`)
    try {
      let data = await Pact.fetch.local({
          pactCode: `(${token}.details ${JSON.stringify(account)})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,0.01,100000000, 28800, creationTime()),
        }, network);
        console.log(data, "gettoken")
        if (data.result.status === "success"){
          // setTokenAccount({...data.result.data, balance: getCorrectBalance(data.result.data.balance)});
          console.log(tokenFromAccount, token, first)
          first ? setTokenFromAccount(data.result.data) : setTokenToAccount(data.result.data)
          console.log(data.result.data)
          return data.result.data
        } else if (data.result.status === "failure"){
          first ? setTokenFromAccount({ account: null, guard: null, balance: 0 }) : setTokenToAccount({ account: null, guard: null, balance: 0 })
          return { account: null, guard: null, balance: 0 }
          console.log("Account does not exist")
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
          console.log(data.result.data)
        } else {
          console.log("Account is not verified")
        }
    } catch (e) {
      console.log(e)
    }
  }

  const createTokenPairLocal = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.create-pair
              ${token0}
              ${token1}
              ""
            )`,
          meta: Pact.lang.mkMeta("", chainId ,0.0001,5000,creationTime(),28800),
          networkId: "testnet04"
      }, network);
      let pair =  data.result.data.account
      try {
        let amount0Decimal = keepDecimal(amountDesired0);
        let amount1Decimal = keepDecimal(amountDesired1);
        let cmd = {
            pactCode: `
            (kswap.exchange.create-pair
                ${token0}
                ${token1}
                ""
            )
            (kswap.exchange.add-liquidity
                ${token0}
                ${token1}
                ${keepDecimal(amountDesired0)}
                ${keepDecimal(amountDesired1)}
                ${keepDecimal(amountDesired0*(1-0.003))}
                ${keepDecimal(amountDesired1*(1-0.003))}
                ${JSON.stringify(account.account)}
                ${JSON.stringify(account.account)}
                (read-keyset 'user-ks)
                )
              )`,
            keyPairs: {
              ...keyPair,
              clist: [
                {name: `${token0}.TRANSFER`, args: [account.account, pair, Number(amountDesired0)]},
                {name: `${token1}.TRANSFER`, args: [account.account, pair, Number(amountDesired1)]},
                {name: `coin.GAS`, args: []}
              ]
            },
            envData: {
              "user-ks": [keyPair.publicKey]
            },
            meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,5000,creationTime(), 600),
            networkId: "testnet04"
          };
        data = await Pact.fetch.local(cmd, network);
        setCmd(cmd);
        console.log(data);
        setLocalRes(data);
        console.log(localRes);
      } catch (e) {
        setLocalRes({});
        console.log(e)
      }
    } catch (e) {
      console.log(e)
    }
  }

  const addLiquidityLocal = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = prompt("please enter your password")
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return
      }
      let pair = await getPairAccount(token0, token1);
      let amount0Decimal = keepDecimal(amountDesired0);
      let amount1Decimal = keepDecimal(amountDesired1);
      let cmd = {
          pactCode: `(kswap.exchange.add-liquidity
              ${token0}
              ${token1}
              ${keepDecimal(amountDesired0)}
              ${keepDecimal(amountDesired1)}
              ${keepDecimal(amountDesired0*(1-0.003))}
              ${keepDecimal(amountDesired1*(1-0.003))}
              ${JSON.stringify(account.account)}
              ${JSON.stringify(account.account)}
              (read-keyset 'user-ks)
            )`,
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: `${token0}.TRANSFER`, args: [account.account, pair, Number(amountDesired0)]},
              {name: `${token1}.TRANSFER`, args: [account.account, pair, Number(amountDesired1)]},
              {name: `coin.GAS`, args: []}
            ]
          },
          envData: {
            "user-ks": account.guard
          },
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
          networkId: "testnet04"
        };
      let data = await Pact.fetch.local(cmd, network);
      setCmd(cmd);
      console.log(data);
      setLocalRes(data);
      console.log(localRes);
    } catch (e) {
      setLocalRes({});
      console.log(e)
    }
  }

  const addLiquidityWallet = async (token0, token1, amountDesired0, amountDesired1) => {
    try {
      let pair = await getPairAccount(token0, token1);
      let amount0Decimal = keepDecimal(amountDesired0);
      let amount1Decimal = keepDecimal(amountDesired1);
      const signCmd = {
        pactCode: `(kswap.exchange.add-liquidity
            ${token0}
            ${token1}
            ${keepDecimal(amountDesired0)}
            ${keepDecimal(amountDesired1)}
            ${keepDecimal(amountDesired0*(1-0.003))}
            ${keepDecimal(amountDesired1*(1-0.003))}
            ${JSON.stringify(account.account)}
            ${JSON.stringify(account.account)}
            (read-keyset 'user-ks)
          )`,
        caps: [
          Pact.lang.mkCap("Gas capability", "Pay gas", "coin.GAS", []),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `${token0}.TRANSFER`, [account.account, pair, Number(amountDesired0)]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `${token1}.TRANSFER`, [account.account, pair, Number(amountDesired1)]),
        ],
        sender: account.account,
        gasLimit: 3000,
        chainId: chainId,
        ttl: 600,
        envData: { "user-ks": account.guard }
      }
      const cmd = await Pact.wallet.sign(signCmd);
      setWalletSuccess(true)
      const res = await Pact.wallet.sendSigned(cmd, network);
      //this is a small hack to get the polling header widget to work
      setLocalRes({ reqKey: res.requestKeys[0] })
      setPolling(true)
      await listen(res.requestKeys[0]);
      setPolling(false)
    } catch (e) {
      alert("you cancelled the TX or you did not have the wallet app open")
      console.log(e)
    }
  }

  const removeLiquidityLocal = async (token0, token1, liquidity) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = prompt("please enter your password")
        privKey = await decryptKey(pw)
      }
      if (privKey.length !== 64) {
        return
      }
      let pairKey = await getPairKey(token0, token1);
      liquidity = keepDecimal(liquidity);
      let pair = await getPairAccount(token0, token1);
      let cmd = {
          pactCode: `(kswap.exchange.remove-liquidity
              ${token0}
              ${token1}
              ${liquidity}
              0.0
              0.0
              ${JSON.stringify(account.account)}
              ${JSON.stringify(account.account)}
              (read-keyset 'user-ks)
              )
            )`,
            networkId: "testnet04",
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: `kswap.tokens.TRANSFER`, args: [pairKey, account.account, pair, Number(liquidity)]},
              {name: `kswap.tokens.TRANSFER`, args: [pairKey, account.account, pair, Number(liquidity)]},
              {name: `coin.GAS`, args: []}
            ]
          },
          envData: {
            "user-ks": account.guard
          },
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
        };
        setCmd(cmd);
        let data = await Pact.fetch.local(cmd, network);
        console.log(data);
        setLocalRes(data);
    } catch (e) {
      console.log(e)
    }
  }

  const removeLiquidityWallet = async (token0, token1, liquidity) => {
    try {
      let pairKey = await getPairKey(token0, token1);
      liquidity = keepDecimal(liquidity);
      let pair = await getPairAccount(token0, token1);
      const signCmd = {
        pactCode:`(kswap.exchange.remove-liquidity
            ${token0}
            ${token1}
            ${liquidity}
            0.0
            0.0
            ${JSON.stringify(account.account)}
            ${JSON.stringify(account.account)}
            (read-keyset 'user-ks)
            )
          )`,
        caps: [
          Pact.lang.mkCap("Gas capability", "Pay gas", "coin.GAS", []),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `kswap.tokens.TRANSFER`, [pairKey, account.account, pair, Number(liquidity)]),
          Pact.lang.mkCap("transfer capability", "Transfer Token to Pool", `kswap.tokens.TRANSFER`, [pairKey, account.account, pair, Number(liquidity)]),
        ],
        sender: account.account,
        gasLimit: 3000,
        chainId: chainId,
        ttl: 600,
        envData: { "user-ks": account.guard }
      }
      const cmd = await Pact.wallet.sign(signCmd);
      setWalletSuccess(true)
      const res = await Pact.wallet.sendSigned(cmd, network);
      //this is a small hack to get the polling header widget to work
      setLocalRes({ reqKey: res.requestKeys[0] })
      setPolling(true)
      await listen(res.requestKeys[0]);
      setPolling(false)
    } catch (e) {
      alert("you cancelled the TX or you did not have the wallet app open")
      console.log(e)
    }
  }

  const getPairAccount = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(at 'account (kswap.exchange.get-pair ${token0} ${token1}))`,
          meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          setPairAccount(data.result.data);
          return data.result.data;
          console.log("Pair Account is set to", data.result.data);
        } else {
          console.log("Pair Account is not verified")
        }
        console.log(data);
    } catch (e) {
      console.log(e)
    }
  }

  const getPair = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.get-pair ${token0} ${token1})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          setPair(data.result.data);
          console.log("Pair is set to", data.result.data);
          return data.result.data;
        } else {
          return null;
          console.log("Pair does not exist")
        }
        console.log(data);
    } catch (e) {
      console.log('fail')
      console.log(e)
    }
  }


  const getPairKey = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.exchange.get-pair-key ${token0} ${token1})`,
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          // setPairKey(data.result.data);
          return data.result.data;
          console.log("Pair Account is set to", data.result.data);
        } else {
          console.log("Pair Account is not verified")
        }
        console.log(data);
    } catch (e) {
      console.log(e)
    }
  }

  const getPairAccountBalance = async (token0, token1, account) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(kswap.tokens.get-balance (kswap.exchange.get-pair-key ${token0} ${token1}) ${JSON.stringify(account)})`,
          meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          console.log("Success", data.result.data);
          setPairAccountBalance(data.result.data);
        } else {
          console.log("Fail", data)
          // setPairAccountBalance(null);
          console.log("Pair Account is not verified")
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPairListAccountBalance = async (account) => {
    let pairList = await Promise.all(Object.values(pairTokens).map(async pair => {
      try {
        let data = await Pact.fetch.local({
            pactCode: `
            (use kswap.exchange)
            (let*
              (
                (p (get-pair ${pair.token0.name} ${pair.token1.name}))
                (reserveA (reserve-for p ${pair.token0.name}))
                (reserveB (reserve-for p ${pair.token1.name}))
                (totalBal (kswap.tokens.total-supply (kswap.exchange.get-pair-key ${pair.token0.name} ${pair.token1.name})))
                (acctBal (kswap.tokens.get-balance (kswap.exchange.get-pair-key ${pair.token0.name} ${pair.token1.name}) ${JSON.stringify(account)}))
              )[acctBal totalBal reserveA reserveB (* reserveA (/ acctBal totalBal))(* reserveB (/ acctBal totalBal))])
             `,
            meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
          }, network);
        if (data.result.status === "success"){
          console.log("Success", data.result.data, "pair list", pairListAccount);
          return {...pair,
              balance: data.result.data[0],
              supply: data.result.data[1],
              reserves:[data.result.data[2],  data.result.data[3]],
              pooledAmount: [data.result.data[4],  data.result.data[5]]
            }
        } else {
          console.log("Fail pair list", data)
          console.log("Pair Account is not verified")
        }
      } catch (e) {
        console.log(e)
      }
    }))
    console.log(pairList)
    setPairListAccount(pairList);
  }

  const getPairList = async () => {
    let pairList = await Promise.all(Object.values(pairTokens).map(async pair => {
      try {
        let data = await Pact.fetch.local({
            pactCode: `
            (use kswap.exchange)
            (let*
              (
                (p (get-pair ${pair.token0.name} ${pair.token1.name}))
                (reserveA (reserve-for p ${pair.token0.name}))
                (reserveB (reserve-for p ${pair.token1.name}))
                (totalBal (kswap.tokens.total-supply (kswap.exchange.get-pair-key ${pair.token0.name} ${pair.token1.name})))
              )[totalBal reserveA reserveB])
             `,
            meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
          }, network);
        if (data.result.status === "success"){
          console.log("Success", data.result.data, "pair list");
          return {...pair,
              supply: data.result.data[0],
              reserves:[data.result.data[1],  data.result.data[2]]
            }
        } else {
          console.log("Fail pair list", data)
          console.log("Pair Account is not verified")
        }
      } catch (e) {
        console.log(e)
      }
    }))
    console.log(pairList)
    setPairList(pairList);
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
           meta: Pact.lang.mkMeta("account", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          console.log("succeeded, update reserve", data.result.data)
          await setPairReserve({token0: data.result.data[0].decimal? data.result.data[0].decimal:  data.result.data[0], token1: data.result.data[1].decimal? data.result.data[1].decimal:  data.result.data[1]});
        } else {
          console.log("Failed")
          await setPairReserve({});
        }
    } catch (e) {
      console.log(e)
    }
  }

  const getPooledAmount = async (pairKey, token0, token1, account) => {
    console.log(pairKey, token0, token1, account)
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
           meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        console.log(data)
        let balance0= data.result.data[0].decimal?data.result.data[0].decimal :data.result.data[0] ;
        let balance1= data.result.data[1].decimal?data.result.data[1].decimal :data.result.data[1] ;
        setPoolBalance([balance0, balance1]);
        if (data.result.status === "success"){
          console.log(data, " pooledamount")
        } else {
          console.log("Failed")
        }
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
           meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
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
          ${keepDecimal(token0.amount)}
          ${keepDecimal(token1.amount*(1-slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          ${keepDecimal(token1.amount)}
          ${keepDecimal(token0.amount*(1+slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const cmd = {
          pactCode: (isSwapIn ? inPactCode : outPactCode),
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: `${token0.address}.TRANSFER`, args: [account.account, pair, Number(token0.amount*(1+slippage))]},
            ]
          },
          envData: {
            "user-ks": account.guard
          },
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
          networkId: "testnet04",
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
      }
      setCmd(cmd);
      console.log(cmd)
      let data = await Pact.fetch.send(cmd, network);
      console.log(data);
    } catch (e) {
      console.log(e)
    }
  }

  const swapLocal = async (token0, token1, isSwapIn) => {
    try {
      let privKey = signing.key
      if (signing.method === 'pk+pw') {
        const pw = prompt("please enter your password")
        privKey = await decryptKey(pw)
      }
      console.log(privKey)
      if (privKey.length !== 64) {
        return
      }
      const ct = creationTime();
      console.log(account.account)
      let pair = await getPairAccount(token0.address, token1.address);
      const inPactCode = `(kswap.exchange.swap-exact-in
          ${keepDecimal(token0.amount)}
          ${keepDecimal(token1.amount*(1-slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          ${keepDecimal(token1.amount)}
          ${keepDecimal(token0.amount*(1+slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const cmd = {
          pactCode: (isSwapIn ? inPactCode : outPactCode),
          keyPairs: {
            publicKey: account.guard.keys[0],
            secretKey: privKey,
            clist: [
              {name: "coin.GAS", args: []},
              {name: `${token0.address}.TRANSFER`, args: [account.account, pair, parseFloat(keepDecimal(token0.amount*(1+slippage)))]},
            ]
          },
          envData: {
            "user-ks": account.guard
          },
          networkId: "testnet04",
          meta: Pact.lang.mkMeta(account.account, chainId, 0.0001, 3000, ct, 600),
      }
      setCmd(cmd);
      console.log(cmd)
      let data = await Pact.fetch.local(cmd, network);
      setLocalRes(data);
      console.log(data);
      return data;
    } catch (e) {
      setLocalRes({});
      return -1
      console.log(e)
    }
  }

  const swapWallet = async (token0, token1, isSwapIn) => {
    try {
      console.log(Pact.lang.mkCap("transfer capability", "trasnsfer token in", `${token0.address}.TRANSFER`, [account.account, pair.account, parseFloat(keepDecimal(token0.amount*(1+slippage)))]))
      const inPactCode = `(kswap.exchange.swap-exact-in
          ${keepDecimal(token0.amount)}
          ${keepDecimal(token1.amount*(1-slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const outPactCode = `(kswap.exchange.swap-exact-out
          ${keepDecimal(token1.amount)}
          ${keepDecimal(token0.amount*(1+slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          )
        )`
      const signCmd = {
        pactCode: (isSwapIn ? inPactCode : outPactCode),
        caps: [
          Pact.lang.mkCap("Gas capability", "pay gas", "coin.GAS", []),
          Pact.lang.mkCap("transfer capability", "trasnsfer token in", `${token0.address}.TRANSFER`, [account.account, pair.account, parseFloat(keepDecimal(token0.amount*(1+slippage)))]),
        ],
        sender: account.account,
        gasLimit: 3000,
        chainId: chainId,
        ttl: 600,
        envData: { "user-ks": account.guard }
      }
      const cmd = await Pact.wallet.sign(signCmd);
      setWalletSuccess(true)
      const res = await Pact.wallet.sendSigned(cmd, network);
      //this is a small hack to get the polling header widget to work
      setLocalRes({ reqKey: res.requestKeys[0] })
      setPolling(true)
      await listen(res.requestKeys[0]);
      setPolling(false)
    } catch (e) {
      alert("you cancelled the TX or you did not have the wallet app open")
      console.log(e)
    }

  }

  const swapSend = async () => {
    setPolling(true)
    try {
      console.log(cmd)
      const data = await Pact.fetch.send(cmd, network)
      toastId.current = notificationContext.showNotification({
              title: 'Transaction Pending',
              message: data.requestKeys[0],
              type: STATUSES.INFO,
              autoClose: 92000,
              hideProgressBar: false,
              // onOpen: (value) => {
              //   setToastId(value.toastProps.toastId);
              // }
            }
      )
      console.log(data)
      await listen(data.requestKeys[0]);
      setPolling(false)
    } catch (e) {
      setPolling(false)
      console.log(e)
    }
  }

  const listen = async (reqKey) => {
    const res = await Pact.fetch.listen({listen: reqKey}, network);
    console.log(res);
    setSendRes(res);
    if (res.result.status === 'success') {
      notificationContext.showNotification({
              title: 'Transaction Success!',
              message: 'Check it out in the block explorer',
              type: STATUSES.SUCCESS,
              onClose: async () => {
                await toast.dismiss(toastId)
                await window.open(
                  `https://explorer.chainweb.com/testnet/tx/${res.reqKey}`,
                  "_blank",
                  'noopener,noreferrer'
                );
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
                  `https://explorer.chainweb.com/testnet/tx/${res.reqKey}`,
                  "_blank",
                  'noopener,noreferrer'
                );
              },
              onOpen: async (value) => {
                await toast.dismiss(toastId.current)
              }
            }
      )
    }
  }

  const getAccountTokenList = async (account) => {
    let list = await tokens();
    list =
    list
    .map(pair =>  {
      return `(kswap.tokens.get-balance ${JSON.stringify(pair)} ${JSON.stringify(account)})`;
    })

    try {
      let data = await Pact.fetch.local({
          pactCode: list[0],
          meta: Pact.lang.mkMeta("account", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){

          console.log("Success", data.result.data);
        } else {
          console.log("Fail", data)
          // setPairAccountBalance(null);
          console.log("Pair Account is not verified")
        }
    } catch (e) {
      console.log(e)
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
    console.log(pk, pw)
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

  return (
    <PactContext.Provider
      value={{
        tokens,
        getAccountTokenList,
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
        pairListAccount
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
