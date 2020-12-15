import React, { useState, createContext, useEffect } from 'react';
import Pact from "pact-lang-api";

const keepDecimal = decimal => {
  decimal = parseFloat(decimal).toPrecision(15)
  const num = decimal.toString().indexOf('.') === -1 ? `${decimal}.0` : decimal
  return num
}
export const PactContext = createContext();

const savedAcct = localStorage.getItem('acct');
const savedPrivKey = localStorage.getItem('pk');
const savedNetwork = localStorage.getItem('network');
const savedSlippage = localStorage.getItem('slippage')

const network = "https://us1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact";
const chainId = "0";
const creationTime = () => Math.round((new Date).getTime()/1000)-10;

export const PactProvider = (props) => {

  // const network = "http://localhost:9001"
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
  const creationTime = () => Math.round((new Date).getTime()/1000)-10;
  const [supplied, setSupplied] = useState(false);
  const [slippage, setSlippage] = useState((savedSlippage ? savedSlippage : 0.50));
  const [liquidityProviderFee, setLiquidityProviderFee] = useState(0.003);
  const [cmd, setCmd] = useState(null);
  const [localRes, setLocalRes] = useState({});
  const [polling, setPolling] = useState(false);
  const tokenPrice = {
    "KDA": 1,
    "ABC": 1.05
  }

  useEffect(() => {
    pairReserve ? setRatio(pairReserve['token0']/pairReserve['token1']) : setRatio(NaN)
  }, [pairReserve]);

  useEffect(() => {
    if (account.account) setVerifiedAccount(account.account)
  }, [])

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
        setTokenAccount({...data.result.data, balance: getCorrectBalance(data.result.data.balance)});
        if (data.result.status === "success"){
          first ? setTokenFromAccount(data.result.data) : setTokenToAccount(data.result.data)
          console.log(data.result.data)
          return data.result.data
        } else {
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
          pactCode: `(swap.tokens.total-supply (get-pair-key ${token0} ${token1}))`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", chainId ,0.01,100000000, 28800, creationTime()),
        }, network);
        if (data.result.status === "success"){
          setTokenAccount(data.result.data);
          console.log(data.result.data)
          console.log("Account is set to ", account);
        } else {
          console.log("Account is not verified")
        }
    } catch (e) {
      console.log(e)
    }
  }

  const createTokenPair = async (account, token0, token1, amountDesired0, amountDesired1) => {
    try {
      let data = await Pact.fetch.send({
          pactCode: `(swap.exchange.create-pair
              ${token0}
              ${token1}
              ""
            )`,
          keyPairs: keyPair,
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(),28800),
          networkId: "testnet04"
        }, network);
      Pact.fetch.listen({listen: data.requestKeys[0]}, network)
      .then(() => {
        addLiquidity(account, token0, token1, amountDesired0, amountDesired1);
      })
    } catch (e) {
      console.log(e)
    }
  }

  const addLiquidity = async (account, token0, token1, amountDesired0, amountDesired1) => {
    try {
      let pair = await getPairAccount(token0, token1);
      console.log(pair)
      let data = await Pact.fetch.send({
          pactCode: `(swap.exchange.add-liquidity
              ${token0}
              ${token1}
              ${keepDecimal(amountDesired0)}
              ${keepDecimal(amountDesired1)}
              ${keepDecimal(amountDesired0*(1-slippage))}
              ${keepDecimal(amountDesired1*(1-slippage))}
              ${JSON.stringify(account)}
              ${JSON.stringify(account)}
              (read-keyset 'user-ks)
              (at 'block-time (chain-data))
            )`,
          keyPairs: {
            ...keyPair,
            clist: [
              {name: `${token0}.TRANSFER`, args: [account, pair, Number(amountDesired0)]},
              {name: `${token1}.TRANSFER`, args: [account, pair, Number(amountDesired1)]},
              {name: `coin.GAS`, args: []}
            ]
          },
          envData: {
            "user-ks": [keyPair.publicKey]
          },
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
          networkId: "testnet04"
        }, network);
        console.log(data);
    } catch (e) {
      console.log(e)
    }
  }

  const removeLiquidity = async (account, token0, token1, liquidity) => {
    try {
      let pairKey = await getPairKey(token0, token1);
      let pair = await getPairAccount(token0, token1);
      let data = await Pact.fetch.send({
          pactCode: `(swap.exchange.remove-liquidity
              ${token0}
              ${token1}
              ${keepDecimal(liquidity)}
              0.0
              0.0
              ${JSON.stringify(account)}
              ${JSON.stringify(account)}
              (read-keyset 'user-ks)
              (at 'block-time (chain-data))
            )`,
            networkId: "testnet04"
,
          keyPairs: {
            ...keyPair,
            clist: [
              {name: `swap.tokens.TRANSFER`, args: [pairKey, account, pair, Number(liquidity)]},
              {name: `swap.tokens.TRANSFER`, args: [pairKey, account, pair, Number(liquidity)]},
              {name: `coin.GAS`, args: []}
            ]
          },
          envData: {
            "user-ks": [keyPair.publicKey]
          },
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        console.log(data);
    } catch (e) {
      console.log(e)
    }
  }

  const getPairAccount = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(at 'account (swap.exchange.get-pair ${token0} ${token1}))`,
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
          pactCode: `(swap.exchange.get-pair ${token0} ${token1})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        console.log(data)
        if (data.result.status === "success"){
          setPair(data.result.data);
          return data.result.data;
          console.log("Pair is set to", data.result.data);
        } else {
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
          pactCode: `(swap.exchange.get-pair-key ${token0} ${token1})`,
          meta: Pact.lang.mkMeta(account.account, chainId ,0.0001,3000,creationTime(), 600),
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

  const getPairAccountBalance = async (token0, token1, account) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(swap.tokens.get-balance (swap.exchange.get-pair-key ${token0} ${token1}) ${JSON.stringify(account)})`,
          meta: Pact.lang.mkMeta("", chainId ,0.0001,3000,creationTime(), 600),
        }, network);
        if (data.result.status === "success"){
          console.log("Success", data.result.data)
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

  const getReserves = async (token0, token1) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `
          (use swap.exchange)
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
          console.log("succeeded, update reserve")
          console.log(data.result.data);
          setPairReserve({token0: getCorrectBalance(data.result.data[0]), token1: getCorrectBalance(data.result.data[1])});
          console.log(pairReserve, " reserve")
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

      const inPactCode = `(swap.exchange.swap-exact-in
          ${keepDecimal(token0.amount)}
          ${keepDecimal(token1.amount*(1-slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          (at 'block-time (chain-data))
        )`
      const outPactCode = `(swap.exchange.swap-exact-out
          ${keepDecimal(token1.amount)}
          ${keepDecimal(token0.amount*(1+slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          (at 'block-time (chain-data))
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
      const ct = creationTime();
      let pair = await getPairAccount(token0.address, token1.address);
      const inPactCode = `(swap.exchange.swap-exact-in
          ${keepDecimal(token0.amount)}
          ${keepDecimal(token1.amount*(1-slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          (at 'block-time (chain-data))
        )`
      const outPactCode = `(swap.exchange.swap-exact-out
          ${keepDecimal(token1.amount)}
          ${keepDecimal(token0.amount*(1+slippage))}
          [${token0.address} ${token1.address}]
          ${JSON.stringify(account.account)}
          ${JSON.stringify(account.account)}
          (read-keyset 'user-ks)
          (at 'block-time (chain-data))
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
          networkId: "testnet04",
          meta: Pact.lang.mkMeta(account.account, chainId, 0.0001, 3000, ct, 600),
      }
      setCmd(cmd);
      console.log(cmd)
      let data = await Pact.fetch.local(cmd, network);
      setLocalRes(data);
      console.log(data);
    } catch (e) {
      setLocalRes({});
      console.log(e)
    }
  }

  const swapSend = async () => {
    setPolling(true)
    try {
      console.log(cmd)
      const data = await Pact.fetch.send(cmd, network)
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
    console.log(res)
    if (res.result.status === 'success') {
      console.log('success send')
    } else {
      console.log('fail send')
    }
  }

  const getTokenPrice = (token) => {
    return tokenPrice[token];
  }

  const getRatio = (toToken, fromToken) => {
    return pairReserve["token0"]/pairReserve["token1"]
  }

  const storePrivKey = async (pk) => {
    await setPrivKey(pk)
    await localStorage.setItem('pk', pk);
  }

  return (
    <PactContext.Provider
      value={{
        account,
        setVerifiedAccount,
        getTokenAccount,
        getTokenPrice,
        getRatio,
        supplied,
        setSupplied,
        addLiquidity,
        removeLiquidity,
        createTokenPair,
        pairAccount,
        getPairAccount,
        getPairAccountBalance,
        privKey,
        storePrivKey,
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
        polling
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
