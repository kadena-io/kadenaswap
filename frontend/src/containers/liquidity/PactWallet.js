import React, { useEffect, useContext, useState } from 'react';
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

  useEffect( async () => {
    await pact.getPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name);
  }, []);

  const buttonStatus = () => {
    let status = {
      0: {msg: "Connect Wallet", status: false},
      1: {msg: "Enter An Amount", status: true},
      2: {msg: "Supply", status: true},
      3: {msg: (token) => `Insufficient ${token} Balance`,status: false},
      4: {msg:"Pair Already Exists", status: false},
      5: {msg: "Select different tokens", status: false}
    }
    if (!fromValues.amount && !toValues.amount) return status[1];
    else if (props.liquidityView==="Create A Pair" && pact.pair) return status[4];
    else if (!pact.account.account || (fromValues.amount > pact.account.balance)) return status[3];
    else if (toValues.amount > pact.tokenAccount.balance) return status[3];
    else if (fromValues.coin === toValues.coin) return status[5];
    else return status[2];
  }

  const supply = async () => {
    if (open) {
      if (props.liquidityView==="Create A Pair"){
        // setLoading(true)
        await pact.createTokenPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount).then(console.log)
        // setLoading(false)
        // setShowTxModal(true)
      } else{
        setLoading(true)
        await pact.addLiquidity(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
        setLoading(false)
        // setShowTxModal(true)
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
            disabled={!buttonStatus().status}
            buttonStyle={{ marginTop: 24, marginRight: 0 }}>
            {buttonStatus().msg}
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
            <List.Item>Share of the Pool : {reduceBalance(pact.share(fromValues.amount)*100)}%</List.Item>
          </List>
        </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button color='black'
            loading={loading}
            onClick={supply}>
            Supply
          </Button>
        </Modal.Actions>
      </Modal>
  )
}

export default PactWallet
