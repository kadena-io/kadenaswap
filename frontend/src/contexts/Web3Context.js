import React, { Component, createContext } from 'react';

import Web3 from 'web3';
import Web3Modal from 'web3modal';
import WalletConnectProvider from '@walletconnect/web3-provider';
import Fortmatic from 'fortmatic';

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
	network: 'mainnet',
	cacheProvider: false,
	providerOptions,
});

export const Web3Context = createContext();

class Web3ContextProvider extends Component {
	state = {
		address: '',
	};

	connectAccount = async () => {
		web3Modal.clearCachedProvider();

		const provider = await web3Modal.connect();
		const web3 = new Web3(provider);
		const accounts = await web3.eth.getAccounts();

		this.setState({ address: accounts[0], provider, web3 });
	};

	render() {
		return (
			<Web3Context.Provider
				value={{
          ...this.state,
          connectAccount: this.connectAccount
        }}
			>
				{this.props.children}
			</Web3Context.Provider>
		);
	}
}

export default Web3ContextProvider;
