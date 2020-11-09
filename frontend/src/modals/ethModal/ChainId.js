import React from 'react';
import { useWeb3React } from '@web3-react/core'

export default function ChainId() {
  const { chainId } = useWeb3React()

  return (
    <>
      <span>Chain Id</span>
      <span role="img" aria-label="chain">
        
      </span>
      <span>{chainId ?? ''}</span>
    </>
  )
}
