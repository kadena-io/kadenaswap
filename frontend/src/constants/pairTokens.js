import React from 'react';
import { ReactComponent as EthereumIcon } from '../assets/images/crypto/ethereum.svg';
import { ReactComponent as KadenaIcon } from '../assets/images/crypto/kadena-logo.svg';

export default {
  "coin:kswap.abc": {
    name: "coin:kswap.abc",
    pairAccount: "",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'kswap.abc', code: 'ABC', icon: <KadenaIcon />}
  },
  "coin:kswap.xyz": {
    name: "coin:kswap.xyz",
    pairAccount: "",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'kswap.xyz', code: 'XYZ', icon: <KadenaIcon />}
  },
  "coin:free.abc": {
    name: "coin:free.abc",
    pairAccount: "",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'free.abc', code: 'FABC', icon: <KadenaIcon />}
  }
}
