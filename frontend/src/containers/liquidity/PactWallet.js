import React, { useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Button from '../../components/shared/Button';
import Input from '../../components/shared/Input';
import { PactContext } from '../../contexts/PactContext'
import { Statistic, List, Divider } from 'semantic-ui-react'
import cryptoCurrencies from '../../constants/cryptoCurrencies';
const reduceBalance = (num) => {
  if (num.decimal) {
    num = num.decimal;
  }
  if (num.toString().length>5) return num.toString().slice(0,5);
  else return num.toString();
}

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
      3: (token) => `Insufficient ${token} Balance`,
      4: "Pair Already Exists",
      5: "Select different tokens"
    }
    if (fromValues.amount === 0 && toValues.amount === 0) return status[1];
    else if (!pact.account.account || (fromValues.amount > pact.account.balance)) return status[3](fromValues.coin);
    else if (toValues.amount > pact.tokenAccount.balance) return status[3](toValues.coin);
    else if (fromValues.coin === toValues.coin) return status[5];
    else return status[2];
  }

  const share = 0;

  const buttonDisabled = () => {
    if (fromValues.amount===0 && toValues.amount===0) return false;
    if (!pact.account.account) return false;
    else if (fromValues.amount > pact.account.balance) return true;
    else if (toValues.amount > pact.tokenAccount.balance) return true;
    else if (fromValues.coin === toValues.coin) return true;
    else return false;
  }

  const supply = async () => {
    if (open) {
      if (props.liquidityView==="Create A Pair"){
        let pair = await pact.getPair();
        if (!pair){
          console.log("Pair Already Exists");
        } else {
          pact.createTokenPair(pact.account.account, cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount).then(console.log)
        }
      } else{
        pact.addLiquidity(pact.account.account, cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
      }
    } else {openConfirmSupply();}
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
            <List.Item>Share of the Pool : {share}</List.Item>
          </List>
        </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button color='black'
            onClick={supply}>
            Supply
          </Button>
        </Modal.Actions>
      </Modal>
  )
}

export default PactWallet
