import React, { useState, createContext, useEffect } from 'react';
import { Connectors } from 'web3-react'
import Web3 from 'web3';
import Web3Modal from 'web3modal';
import WalletConnectProvider from '@walletconnect/web3-provider';
import Fortmatic from 'fortmatic';
import { useWallet } from 'use-wallet'

export const EthContext = createContext();

export const EthProvider = (props) => {

  const wallet = useWallet()

  const [web3, setWeb3] = useState(null);
  const [accts, setAccts] = useState([]);
  const [address, setAddress] = useState("");
  const [loading, setLoading] = useState(false);
  const [balance, setBalance] = useState(0)
  const [provider, setProvider] = useState()

  const providerOptions = {
  	walletconnect: {
  		package: WalletConnectProvider,
  		options: {
  			infuraId: process.env.REACT_APP_INFURA_ID,
  		},
  	},
  	fortmatic: {
  		package: Fortmatic,
  		options: {
  			key: process.env.REACT_APP_FORTMATIC_ID,
  		},
  	},
  };

  const web3Modal = new Web3Modal({
  	// network: 'mainnet',
  	cacheProvider: false,
  	providerOptions,
  });


  useEffect(() => {
    (async function _getEthEnv() {
      try {
        let web3Inst = await new Web3(window.ethereum)
        await setWeb3(web3Inst);
        const as = await web3Inst.eth.getAccounts();
        await setAccts(as)
        await setAddress(as[0]);
        let bal = await web3Inst.eth.getBalance(as[0]);
        bal = web3Inst.utils.fromWei(bal)
        console.log(bal)
        await setBalance(bal);
        console.log(address)
      } catch (e) {
        console.log(e)
        console.log('did not find web3 instance')
        await setAccts([])
        await setAddress("")
      }
    })();
  }, []);

  const connectAccount = async () => {
		await web3Modal.clearCachedProvider();
		const provider = await web3Modal.connect();
		const web3I = await new Web3(provider);
    console.log(web3I)
    await setWeb3(web3I)
    console.log(web3)
    await getAccounts();
	};

  const getEth = async () => {
    try {
      let web3Inst = await new Web3(window.ethereum)
      await setWeb3(web3Inst);
      console.log(accts)
    } catch (e) {
      console.log(e)
    }
  }

  const getAccounts = async () => {
    try {
      const as = await web3.eth.getAccounts();
      await setAccts(as);
      await setAddress(as[0]);
      await getBalance();
    } catch (e) {
      // setAccts([]);
      console.log(e)
    }
  }

  const getBalance = async () => {
    try {
      const bal = await web3.eth.getBalance();
      console.log(bal)
      await setBalance(bal);
    } catch (e) {
      console.log(e)
    }
  }

  const connectMetaMask = async () => {
    await wallet.connect();
    console.log(wallet.status)
    await getAccounts();
    console.log(accts);
  }

  const disconnectWallet = async () => {
    //NOTE: this does not disconnect the wallet fully
    //  only changes provider
    //    user must manually disconnect wallet for secure session to end
    console.log('disc')
    await wallet.reset();
    await setAccts([]);
    await setAddress("")
    console.log(wallet.status)
    //VERIFY WALLET IS DISCONNECTED
    // await setAccts([])
  }

  return (
    <EthContext.Provider
      value={{
        accts,
        web3,
        balance,
        connectMetaMask,
        disconnectWallet,
        getBalance,
        address,
        connectAccount
      }}
    >
      {props.children}
    </EthContext.Provider>
  );
};

export const EthConsumer = EthContext.Consumer;

export const withEthContext = (Component) => (props) => (
  <EthConsumer>{(providerProps) => <Component {...props} sessionContextProps={providerProps} />}</EthConsumer>
);
