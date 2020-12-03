import React, { useState, createContext, useEffect } from 'react';
import Pact from "pact-lang-api";

const keepDecimal = decimal =>{
  const num = decimal.toString().indexOf('.') === -1 ? `${decimal}.0` : decimal
  return num
}
const slippageTolerance = 0.003;
export const PactContext = createContext();

const savedAcct = localStorage.getItem('acct');
const savedPrivKey = localStorage.getItem('pk')

export const PactProvider = (props) => {

  const network = "http://localhost:9001"
  const [account, setAccount] = useState((savedAcct ? JSON.parse(savedAcct) : {account: null, guard: null, balance: 0}));
  const [tokenAccount, setTokenAccount] = useState({account: null, guard: null, balance: 0});
  const [privKey, setPrivKey] = useState((savedPrivKey ? savedPrivKey : ""))
  const [tokenFromAccount, setTokenFromAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenToAccount, setTokenToAccount] = useState({account: null, guard: null, balance: 0});
  const [tokenList, setTokenList] = useState({tokens: []})
  const [pairAccount, setPairAccount] = useState("")
  const [pairReserve, setPairReserve] = useState("")
  const [pair, setPair] = useState("")
  const [pairAccountBalance, setPairAccountBalance] = useState(null);
  const creationTime = () => Math.round((new Date).getTime()/1000)-10;
  const [supplied, setSupplied] = useState(false)
  const tokenPrice = {
    "KDA": 1,
    "ABC": 1.05
  }

  const setVerifiedAccount = async (accountName) => {
    try {
      let data = await Pact.fetch.local({
          pactCode: `(coin.details ${JSON.stringify(accountName)})`,
          meta: Pact.lang.mkMeta("", "3" ,0.01,100000000, 28800, creationTime()),
        }, network);
        console.log(data)
        if (data.result.status === "success"){
          await localStorage.setItem('acct', JSON.stringify(data.result.data));
          setAccount(data.result.data);
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
          meta: Pact.lang.mkMeta("", "3" ,0.01,100000000, 28800, creationTime()),
        }, network);
        console.log(data, "gettoken")
        console.log(data.result.data)
        setTokenAccount(data.result.data);
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
          meta: Pact.lang.mkMeta("", "3" ,0.01,100000000, 28800, creationTime()),
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
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
              ${keepDecimal(amountDesired0*(1-slippageTolerance))}
              ${keepDecimal(amountDesired1*(1-slippageTolerance))}
              ${JSON.stringify(account)}
              ${JSON.stringify(account)}
              (read-keyset 'user-ks)
              (at 'block-time (chain-data))
            )`,
          keyPairs: {
            publicKey: "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca",
            secretKey: "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898",
            clist: [
              {name: `${token0}.TRANSFER`, args: [account, pair, Number(amountDesired0)]},
              {name: `${token1}.TRANSFER`, args: [account, pair, Number(amountDesired1)]}
            ]
          },
          envData: {
            "user-ks": ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"]
          },
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
          keyPairs: {
            publicKey: "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca",
            secretKey: "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898",
            clist: [
              {name: `swap.tokens.TRANSFER`, args: [pairKey, account, pair, Number(liquidity)]},
              {name: `swap.tokens.TRANSFER`, args: [pairKey, account, pair, Number(liquidity)]},
            ]
          },
          envData: {
            "user-ks": ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"]
          },
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
    console.log('getting pair')
    try {
      console.log('getting pairdddd')
      let data = await Pact.fetch.local({
          pactCode: `(swap.exchange.get-pair ${token0} ${token1})`,
          keyPairs: Pact.crypto.genKeyPair(),
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
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
          meta: Pact.lang.mkMeta("", "" ,0,0,0,0),
        }, network);
        if (data.result.status === "success"){
          console.log("succeeded, update reserve")
          await setPairReserve({token0: data.result.data[0], token1: data.result.data[1]});
          console.log(pairReserve, " reserve")
        } else {
          console.log("Failed")
        }
    } catch (e) {
      console.log(e)
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
        pairReserve

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
