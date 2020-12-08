import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as SwapArrowsIcon } from '../assets/images/shared/swap-arrows.svg';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import cryptoCurrencies from '../constants/cryptoCurrencies';
import TokenSelector from '../components/shared/TokenSelector';
import { PactContext } from '../contexts/PactContext';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const RowContainer = styled.div`
  display: flex;
  justify-content: space-between;
  margin: 15px 0px;
`;

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
`;

const SwapContainer = () => {
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [fromValues, setFromValues] = useState({ amount: '', balance: '', coin: '', address: '' });
  const [toValues, setToValues] = useState({ amount: '', balance: '', coin: '', address: '' });
  const [ratio, setRatio] = useState(null);

  const pact = useContext(PactContext);

  // useEffect(() => {
  //   if (tokenSelectorType === 'from') {
  //     console.log('from')
  //     setSelectedToken(fromValues.coin);
  //     if (fromValues.amount !== '' && toValues.amount !== '') {
  //       console.log('both have')
  //       setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
  //     } else if (fromValues.amount !== '') {
  //       console.log('only from')
  //       return setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
  //     }
  //
  //   }
  //   if (tokenSelectorType === 'to') {
  //     console.log('to')
  //     setSelectedToken(toValues.coin);
  //   }
  //   // if (fromValues.amount !== '' && toValues.amount !== '') {
  //   //   console.log('both have')
  //   //   if (tokenSelectorType === 'from') return setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
  //   //   if (tokenSelectorType === 'to') return setFromValues({ ...fromValues, amount: toValues.amount / pact.ratio })
  //   // } else if (fromValues.amount !== '') {
  //   //   console.log('only from')
  //   //   return setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
  //   // } else if (toValues.amount !== '') {
  //   //   console.log('only to')
  //   //   return setFromValues({ ...fromValues, amount: toValues.amount / pact.ratio })
  //   // }
  //   // const getReserves = async () => {
  //   //   if (toValues.coin !== '' && fromValues.coin !== '') {
  //   //     await pact.getReserves(fromValues.address, toValues.address)
  //   //     // if (toValues.amount === '' && fromValues.amount !== '') {
  //   //     //   console.log('hi')
  //   //     //   console.log(fromValues.amount / pact.ratio)
  //   //     //   setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
  //   //     //   // toValues.amount = fromValues.amount / pact.ratio
  //   //     // } else if (fromValues.amount === '' && toValues.amount !== '') {
  //   //     //   console.log(fromValues.amount / pact.ratio)
  //   //     //   setFromValues({ ...fromValues, amount: toValues.amount / pact.ratio })
  //   //     //   // toValues.amount = fromValues.amount / pact.ratio
  //   //     // } else {
  //   //     //
  //   //     // }
  //   //   }
  //   // }
  //   // getReserves();
  //   setSelectedToken(null);
  // }, [tokenSelectorType, fromValues, toValues, pact.ratio]);

  useEffect(() => {
    setToValues({ ...toValues, amount: "" })
    if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio) && fromValues.amount === "") {
      console.log(fromValues)
      setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
    }
  }, [fromValues.amount])

  useEffect(() => {
    console.log('here')
    setFromValues({ ...fromValues, amount: "" })
    if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio) && toValues.amount === "") {
      setFromValues({ ...fromValues, amount: toValues.amount / pact.ratio })
    }
  }, [toValues.amount])

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType]);

  useEffect(() => {
    const getReserves = async () => {
      if (toValues.coin !== '' && fromValues.coin !== '') {
        await pact.getReserves(fromValues.address, toValues.address)
      }
    }
    getReserves();
  }, [fromValues.coin, toValues.coin])


  const swapValues = () => {
    const from = { ...fromValues };
    const to = { ...toValues };
    setFromValues({ ...to });
    setToValues({ ...from });
  };

  const onTokenClick = async ({ crypto }) => {
    let balance;
    if (crypto.name === 'coin') {
      balance = pact.account.balance
    } else {
      let acct = await pact.getTokenAccount(crypto.name, pact.account.account, tokenSelectorType === 'from')
      balance = acct.balance
    }
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
  };

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA account';
    if (!pact.privKey) return 'Enter your KDA account private key';
    if (!fromValues.coin || !toValues.coin) return 'Select tokens';
    if (!fromValues.amount || !toValues.amount) return 'Enter an amount';
    if (fromValues.amount > fromValues.balance) return `Insufficient ${fromValues.coin} balance`
    if (toValues.amount > toValues.balance) return `Insufficient ${toValues.coin} balance`
    return 'SWAP';
  };

  return (
    <Container>
      <TokenSelector
        show={tokenSelectorType !== null}
        selectedToken={selectedToken}
        onTokenClick={onTokenClick}
        onClose={() => setTokenSelectorType(null)}
      />
      <FormContainer title="swap">
        <Input
          leftLabel="from"
          rightLabel={`balance: ${fromValues.balance ?? '-'}`}
          placeholder="enter amount"
          inputRightComponent={
            fromValues.coin ? (
              <InputToken
                icon={cryptoCurrencies[fromValues.coin].icon}
                code={cryptoCurrencies[fromValues.coin].code}
                onClick={() => setTokenSelectorType('from')}
              />
            ) : null
          }
          withSelectButton
          numberOnly
          value={fromValues.amount}
          onSelectButtonClick={() => setTokenSelectorType('from')}
          onChange={async (e, { value }) => {
            await pact.getPair(fromValues.address, toValues.address);
            setFromValues((prev) => ({ ...prev, amount: value }))
          }}
        />
        <ButtonDivider icon={<SwapArrowsIcon />} onClick={swapValues} />
        <Input
          leftLabel="to"
          rightLabel={`balance: ${toValues.balance ?? '-'}`}
          placeholder="enter amount"
          inputRightComponent={
            toValues.coin ? (
              <InputToken
                icon={cryptoCurrencies[toValues.coin].icon}
                code={cryptoCurrencies[toValues.coin].code}
                onClick={() => setTokenSelectorType('to')}
              />
            ) : null
          }
          withSelectButton
          numberOnly
          value={toValues.amount}
          onSelectButtonClick={() => setTokenSelectorType('to')}
          onChange={async (e, { value }) => {
            await pact.getPair(fromValues.address, toValues.address);
            setToValues((prev) => ({ ...prev, amount: value }))
          }}
        />
        {fromValues.amount && fromValues.coin && toValues.amount && toValues.coin && (
          <>
            <RowContainer>
              <Label>price</Label>
              <span>{`${pact.ratio} ${fromValues.coin} per ${toValues.coin}`}</span>
            </RowContainer>
            <RowContainer style={{ marginTop: 10 }}>
              <Label>max slippage</Label>
              <span>0.5%</span>
            </RowContainer>
            {/*
              NEED TO FIGURE OUT PRE AND POST PRICES
              RUN A LOCAL OF THE ACTUAL TRADE AND CHECK NEW PRICES
            <RowContainer style={{ margin: 0 }}>
              <Label>price impact</Label>
              <span>15.16%</span>
            </RowContainer>
            */}
          </>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={getButtonLabel() !== "SWAP"}
          onClick={() => console.log('SWAPPED')}
        >
          {getButtonLabel()}
        </Button>
      </FormContainer>
    </Container>
  );
};

export default SwapContainer;
