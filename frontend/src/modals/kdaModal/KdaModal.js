import React, { useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Input from '../../components/shared/Input';
import Button from '../../components/shared/Button';
import { PactContext } from '../../contexts/PactContext'

export default function Account() {
  const pact = useContext(PactContext);
  const [fromInput, setInputValue] = useState({ account: pact.account.account });
  const [open, setOpen] = React.useState(false)

  const is_hexadecimal = (str) => {
     const regexp = /^[0-9a-fA-F]+$/;
     if (regexp.test(str)) return true;
     else return false;
  }

  const checkKey = (key) => {
     if (key.length !== 64) {
         return false
     } else if (!is_hexadecimal(key)){
         return false
     }
     return true;
  }

  return (
    <>
    <Modal.Content image>
      <Modal.Description>
        <Header>{"Your KDA Account"}</Header>
        <Input
          error={pact.account.account === null}
          value={fromInput.account}
          onChange={async (e, { value }) => {
            setInputValue(value);
            await pact.setVerifiedAccount(value);
          }}
        />
        {(pact.account.account
          ?
            <>
              <Header>{"Account Details"}</Header>
              <span>{JSON.stringify(pact.account.guard)}</span>
            </>
          :
            <Header style={{ color: 'red' }}>{"Account Does Not Exist"}</Header>
        )}
        <Header>{"Your Private Key"}</Header>
        <Input
          value={pact.privKey}
          onChange={(e, { value }) => pact.storePrivKey(value)}
          type='password'
          error={!checkKey(pact.privKey)}
        />
      </Modal.Description>
      </Modal.Content>
      {/*
      <Modal.Actions>
        <Button
          onClick={async () => {
            try{
              await pact.setVerifiedAccount(fromInput.account);
            } catch (e){
              console.log(e)
            }
          }}
        >Connect</Button>
      </Modal.Actions>
      */}
    </>
  )
}
