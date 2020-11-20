import React, { useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Button from '../../components/shared/Button';
import Input from '../../components/shared/Input';
import { PactContext } from '../../contexts/PactContext'
import { Statistic, List, Divider } from 'semantic-ui-react'
function PactWallet(props) {
  const [open, setOpen] = React.useState(false)
  const pact = useContext(PactContext);

  const [fromInput, setInputValue] = useState({ account: '' });
  const {fromValues, toValues} = props;
  const openConfirmSupply= () => {
    pact.setSupplied(true);
    setOpen(false);
  }

  const buttonStatus = () => {
    let status = {
      0: "Connect Wallet",
      1: "Enter An Amount",
      2: "Supply",
      3: (token) => `Insufficient ${token} Balance`
    }
    console.log(pact.tokenAccount)
    if (fromValues.amount === 0 && toValues.amount === 0) return status[1];
    else if (fromValues.amount > pact.account.balance) return status[3](fromValues.coin);
    else if (toValues.amount > pact.tokenAccount.balance) return status[3](toValues.coin)
    else return status[2];
  }

  const buttonDisabled = () => {
    if (fromValues.amount===0 && toValues.amount===0) return false;
    if (!pact.account.account) return false;
    else if (fromValues.amount > pact.account.balance) return true;
    else if (toValues.amount > pact.tokenAccount.balance) return true;
    else return false;
  }

  return (
      <Modal
        onClose={() => setOpen(false)}
        onOpen={() => setOpen(true)}
        open={open}
        style={{
          width:327
        }}
        trigger={
          <Button
            disabled={buttonDisabled()}
            buttonStyle={{ marginTop: 24, marginRight: 0 }}>
            {buttonStatus()}
          </Button>}
      >
        <Modal.Content image>

        <Modal.Description>
          <Header>You will receive</Header>
          <Statistic>
            <Statistic.Value>1 </Statistic.Value>
            <Statistic.Label>{`${fromValues.coin} / ${toValues.coin} Pool Tokens`}</Statistic.Label>
          </Statistic>
          <Divider/>
          <List>
            <List.Item>{`${fromValues.coin} Deposited: ${fromValues.amount}`}</List.Item>
            <List.Item>{`${toValues.coin} Deposited: ${toValues.amount}`}</List.Item>
            <List.Item>{`Rates:`}</List.Item>
            <List.Item>{`1 ${fromValues.coin} = ${pact.getRatio(toValues.coin, fromValues.coin)} ${toValues.coin}`}</List.Item>
            <List.Item>{`1 ${toValues.coin} = ${pact.getRatio(fromValues.coin, toValues.coin)} ${fromValues.coin}`}</List.Item>
            <List.Item>Share of the Pool : 1</List.Item>
          </List>
        </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button color='black'
            onClick={async () => {
              console.log(pact.supplied,open, "supplied")
              if (open) {
                console.log("supplied")
                pact.addLiquidity(pact.account.account, "coin", "abc", fromValues.amount, toValues.amount)
              } else {openConfirmSupply();}
            }
          }>
            Supply
          </Button>
        </Modal.Actions>
      </Modal>
  )
}

export default PactWallet
