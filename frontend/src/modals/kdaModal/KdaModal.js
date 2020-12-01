import React, { useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Input from '../../components/shared/Input';
import Button from '../../components/shared/Button';
import { PactContext } from '../../contexts/PactContext'

export default function Account() {
  const [fromInput, setInputValue] = useState({ account: '' });
  const [open, setOpen] = React.useState(false)
  const pact = useContext(PactContext);

  return (
    <>
    <Modal.Content image>
      <Modal.Description>
        <Header>{"Your KDA Account"}</Header>
        <Input
          value={pact.account.account}
          onChange={(e, { value }) => setInputValue((prev) => ({ ...prev, account: value }))}
        />
        <Header>{"Your Private Key"}</Header>
        <Input
          value={pact.privKey}
          onChange={(e, { value }) => pact.storePrivKey(value)}
          type='password'
        />
      </Modal.Description>
      </Modal.Content>
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
    </>
  )
}
