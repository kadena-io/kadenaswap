import React from 'react';
import { ReactComponent as EthereumIcon } from '../assets/images/crypto/ethereum.svg';
import { ReactComponent as KadenaIcon } from '../assets/images/crypto/kadena-logo.svg';

export default [
  {
    name: "coin:free.abc",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'free.abc', code: 'ABC', icon: <KadenaIcon />}
  }
]
