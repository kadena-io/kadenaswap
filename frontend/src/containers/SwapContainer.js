import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as SwapArrowsIcon } from '../assets/images/shared/swap-arrows.svg';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import cryptoCurrencies from '../constants/cryptoCurrencies';
import reduceBalance from '../utils/reduceBalance';
import TokenSelector from '../components/shared/TokenSelector';
import { PactContext } from '../contexts/PactContext';
import { throttle, debounce } from "throttle-debounce";

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
  const [inputSide, setInputSide] = useState("")
  const [fromNote, setFromNote] = useState("")
  const [toNote, setToNote] = useState("")

  const pact = useContext(PactContext);

  useEffect(() => {
    if (inputSide === 'from' && fromValues.amount !== "") {
      setToNote("(estimated)")
      setFromNote("")
      setInputSide(null)
      if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio)) {
        if (fromValues.amount.length < 5) {
          throttle(500, setToValues({ ...toValues, amount: fromValues.amount / pact.ratio }))
        } else {
          debounce(500, setToValues({ ...toValues, amount: fromValues.amount / pact.ratio }))
        }
      }
    }
    if (isNaN(pact.ratio) || fromValues.amount === "") {
      setToValues((prev) => ({ ...prev, amount: '' }))
    }
  }, [fromValues.amount])

  useEffect(() => {
    if (inputSide === 'to' && toValues.amount !== "") {
      setFromNote("(estimated)")
      setToNote("")
      setInputSide(null)
      if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio)) {
        if (toValues.amount.length < 5) {
          throttle(500, setFromValues({ ...fromValues, amount: toValues.amount * pact.ratio }))
        } else {
          debounce(500, setFromValues({ ...fromValues, amount: toValues.amount * pact.ratio }))
        }
      }
    }
    if (isNaN(pact.ratio) || toValues.amount === "") {
      setFromValues((prev) => ({ ...prev, amount: '' }))
    }
  }, [toValues.amount])


  useEffect(() => {
    console.log(pact.ratio)
    if (!isNaN(pact.ratio)) {
      if (fromValues.amount !== "" && toValues.amount === "") {
        setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
      } if (fromValues.amount === "" && toValues.amount !== "") {
        setFromValues({ ...fromValues, amount: toValues.amount * pact.ratio })
      } if (fromValues.amount !== "" && toValues.amount !== "")  {
        setToValues({ ...toValues, amount: fromValues.amount / pact.ratio })
      }
    }
  }, [pact.ratio])

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType]);

  useEffect(() => {
    const getReserves = async () => {
      if (toValues.coin !== '' && fromValues.coin !== '') {
        await pact.getPair(fromValues.address, toValues.address);
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
      balance = pact.getCorrectBalance(acct)
    }
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
  };

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA account';
    if (!pact.privKey) return 'Enter your KDA account private key';
    if (!fromValues.coin || !toValues.coin) return 'Select tokens';
    if (isNaN(pact.ratio)) return 'Pair does not exist!'
    if (!fromValues.amount || !toValues.amount) return 'Enter an amount';
    if (fromValues.amount > fromValues.balance) return `Insufficient ${fromValues.coin} balance`
    // if (toValues.amount > toValues.balance) return `Insufficient ${toValues.coin} balance`
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
          leftLabel={`from ${fromNote}`}
          rightLabel={`balance: ${reduceBalance(fromValues.balance) ?? '-'}`}
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
            setInputSide('from')
            setFromValues((prev) => ({ ...prev, amount: value }))
          }}
        />
        <ButtonDivider icon={<SwapArrowsIcon />} onClick={swapValues} />
        <Input
          leftLabel={`to ${toNote}`}
          rightLabel={`balance: ${reduceBalance(toValues.balance) ?? '-'}`}
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
            setInputSide('to')
            setToValues((prev) => ({ ...prev, amount: value }))
          }}
        />
        {!isNaN(pact.ratio) && fromValues.amount && fromValues.coin && toValues.amount && toValues.coin && (
          <>
            <RowContainer>
              <Label>price</Label>
              <span>{`${reduceBalance(pact.ratio)} ${fromValues.coin} per ${toValues.coin}`}</span>
            </RowContainer>
            <RowContainer style={{ marginTop: 5 }}>
              <Label>max slippage</Label>
              <span>{`${pact.slippageTollerance*100}%`}</span>
            </RowContainer>
            <RowContainer style={{ marginTop: 5 }}>
              <Label>liquidity provider fee</Label>
              <span>{`${0.003 * parseFloat(fromValues.amount)} ${fromValues.coin}`}</span>
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
          onClick={() => pact.swapExactIn(
            { amount: fromValues.amount, address: fromValues.address },
            { amount: toValues.amount, address: toValues.address }
          )}
        >
          {getButtonLabel()}
        </Button>
      </FormContainer>
    </Container>
  );
};

export default SwapContainer;
