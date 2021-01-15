import React from 'react';
import { ReactComponent as EthereumIcon } from '../assets/images/crypto/ethereum.svg';
import { ReactComponent as KadenaIcon } from '../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as AbcIcon } from '../assets/images/crypto/abc-logo.svg';
import { ReactComponent as XyzIcon } from '../assets/images/crypto/xyz-logo.svg';

export default {
  "coin:kswap.abc": {
    name: "coin:kswap.abc",
    pairAccount: "",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'kswap.abc', code: 'ABC', icon: <AbcIcon style={{width: 16, height: 16}}/>}
  },
  "coin:kswap.xyz": {
    name: "coin:kswap.xyz",
    pairAccount: "",
    token0:  { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
    token1: { name: 'kswap.xyz', code: 'XYZ', icon: <XyzIcon style={{width: 16, height: 16}}/>}
  },
  "kswap.abc:kswap.xyz": {
    name: "kswap.abc:kswap.xyz",
    pairAccount: "",
    token0:  { name: 'kswap.abc', code: 'ABC', icon: <AbcIcon style={{width: 16, height: 16}}/> },
    token1: { name: 'kswap.xyz', code: 'XYZ', icon: <XyzIcon style={{width: 16, height: 16}}/>}
  }
}
