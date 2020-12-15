import React, { useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Button from '../../components/shared/Button';
import Input from '../../components/shared/Input';
import { PactContext } from '../../contexts/PactContext'
import { Statistic, List, Divider } from 'semantic-ui-react'
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import reduceBalance from '../../utils/reduceBalance';
import TxView from '../../components/shared/TxView';

function PactWallet(props) {
  const [open, setOpen] = React.useState(false)
  const pact = useContext(PactContext);
  const [fromInput, setInputValue] = useState({ account: '' });
  const [loading, setLoading] = useState(false)
  const [showTxModal, setShowTxModal] = useState(false)

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
        setLoading(true)
        await pact.createTokenPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount).then(console.log)
        setLoading(false)
        setShowTxModal(true)
      } else{
        pact.addLiquidity(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
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
      <TxView
        show={showTxModal}
        onClose={() => setShowTxModal(false)}
      />
        <Modal.Content image>

        <Modal.Description>
          <Header>You will receive</Header>
          <Statistic>
            <Statistic.Value>1 </Statistic.Value>
            <Statistic.Label>{`${fromValues.coin} / ${toValues.coin} Pool Tokens`}</Statistic.Label>
          </Statistic>
          <Divider/>
          <List>
            <List.Item>{`${fromValues.coin} Deposited: ${reduceBalance(fromValues.amount)}`}</List.Item>
            <List.Item>{`${toValues.coin} Deposited: ${reduceBalance(toValues.amount)}`}</List.Item>
            <br/>
            <List.Item>{`Rates:`}</List.Item>
            <List.Item>{`1 ${fromValues.coin} = ${reduceBalance(1/pact.ratio)} ${toValues.coin}`}</List.Item>
            <List.Item>{`1 ${toValues.coin} = ${reduceBalance(pact.ratio)} ${fromValues.coin}`}</List.Item>
            <List.Item>Share of the Pool : {reduceBalance(pact.share(fromValues.amount)*100)}</List.Item>
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
