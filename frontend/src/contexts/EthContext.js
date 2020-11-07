import React, { useState, createContext, useEffect } from 'react';
import { Connectors } from 'web3-react'
import Web3 from 'web3';
import { useWallet } from 'use-wallet'
import { useEagerConnect, useInactiveListener } from '../eth/hooks'
import { useWeb3React } from '@web3-react/core'

export const EthContext = createContext();

export const EthProvider = (props) => {

  const context = useWeb3React()

  const { connector, library, chainId, account, activate, deactivate, active, error } = context

  // handle logic to recognize the connector currently being activated
  const [activatingConnector, setActivatingConnector] = useState()
  useEffect(() => {
    if (activatingConnector && activatingConnector === connector) {
      setActivatingConnector(undefined)
    }
  }, [activatingConnector, connector])

  // handle logic to eagerly connect to the injected ethereum provider, if it exists and has granted access already
  const triedEager = useEagerConnect()

  // handle logic to connect in reaction to certain events on the injected ethereum provider, if it exists
  useInactiveListener(!triedEager || !!activatingConnector)

  return (
    <EthContext.Provider
      value={{
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
