import React from 'react';
import { ReactComponent as EthereumIcon } from '../assets/images/crypto/ethereum.svg';
import { ReactComponent as KadenaIcon } from '../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as AbcIcon } from '../assets/images/crypto/abc-logo.svg';
import { ReactComponent as XyzIcon } from '../assets/images/crypto/xyz-logo.svg';

export default {
  KDA: { name: 'coin', code: 'KDA', icon: <KadenaIcon /> },
  ABC: { name: 'kswap.abc', code: 'ABC', icon: <AbcIcon /> },
  XYZ: { name: 'kswap.xyz', code: 'XYZ', icon: <XyzIcon /> }
};
