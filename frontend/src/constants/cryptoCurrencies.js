import React from 'react';
import { tokens } from './tokens'

const tokenData = tokens;

Object.values(tokenData).forEach(token => {
  tokenData[token.name].icon = <img src={tokenData[token.name].icon} style={{width: 16, height: 16}} />
})

export default tokenData;
