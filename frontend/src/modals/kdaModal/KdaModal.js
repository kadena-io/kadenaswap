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
        onChange={(e, { value }) => setInputValue((prev) => ({ ...prev, account: value }))}
        />
      </Modal.Description>
      </Modal.Content>
      <Modal.Actions>
        <Button
          onClick={async () => {
            try{
              // (account, token0, token1, amountDesired0, amountDesired1, amountMin0, amountMin1)
              // pact.addLiquidity("user1", "coin", "abc", 10.1, 20.1, 5.1, 5.1);
              pact.getTokenAccount("abc", fromInput.account);
              pact.setVerifiedAccount(fromInput.account);
            } catch (e){
              console.log(e)
            }
          }}
        >Connect</Button>
      </Modal.Actions>
    </>
  )
}
