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
import TxView from '../components/shared/TxView';
import SigningView from '../components/shared/SigningView';
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
  const [showTxModal, setShowTxModal] = useState(false)
  const [loading, setLoading] = useState(false)
  const [fetchingPair, setFetchingPair] = useState(false)

  const pact = useContext(PactContext);

  useEffect(() => {
    if (!isNaN(fromValues.amount)) {
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
    }
  }, [fromValues.amount])

  useEffect(() => {
    if (!isNaN(toValues.amount)) {
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
    }
  }, [toValues.amount])

  useEffect(() => {
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
        setFetchingPair(true)
        await pact.getPair(fromValues.address, toValues.address);
        await pact.getReserves(fromValues.address, toValues.address);
        setFetchingPair(false)
      }
    }
    getReserves();
  }, [fromValues.coin, toValues.coin])

  useEffect(() => {
    if (pact.walletSuccess) {
      setLoading(false)
      setFromValues({ amount: '', balance: '', coin: '', address: '' });
      setToValues({ amount: '', balance: '', coin: '', address: '' })
      pact.setWalletSuccess(false)
    }
  }, [pact.walletSuccess])


  const swapValues = () => {
    const from = { ...fromValues };
    const to = { ...toValues };
    setFromValues({ ...to });
    setToValues({ ...from });
    if (toNote === "(estimated)") {
      setFromNote("(estimated)");
      setToNote("");
    }
    if (fromNote === "(estimated)") {
      setToNote("(estimated)");
      setFromNote("")
    }
  };

  const onTokenClick = async ({ crypto }) => {
    let balance;
    if (crypto.name === 'coin') {
      balance = pact.account.balance
    } else {
      let acct = await pact.getTokenAccount(crypto.name, pact.account.account, tokenSelectorType === 'from')
      balance = pact.getCorrectBalance(acct.balance)
    }
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: balance, coin: crypto.code, address: crypto.name }));
  };

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA wallet';
    if (!pact.hasWallet()) return 'Set signing method in wallet';
    if (!fromValues.coin || !toValues.coin) return 'Select tokens';
    if (fetchingPair) return "Fetching Pair..."
    if (isNaN(pact.ratio)) return 'Pair does not exist!'
    if (!fromValues.amount || !toValues.amount) return 'Enter an amount';
    if (fromValues.amount > fromValues.balance) return `Insufficient ${fromValues.coin} balance`
    return 'SWAP';
  };

  return (
    <Container>
      <TokenSelector
        show={tokenSelectorType !== null}
        selectedToken={selectedToken}
        onTokenClick={onTokenClick}
        fromToken={fromValues.coin}
        toToken={toValues.coin}
        onClose={() => setTokenSelectorType(null)}
      />
      <TxView
        show={showTxModal}
        selectedToken={selectedToken}
        onTokenClick={onTokenClick}
        onClose={() => setShowTxModal(false)}
      />
      <SigningView
        show={pact.sigView}
        // selectedToken={selectedToken}
        // onTokenClick={onTokenClick}
        onClose={() => pact.setSigView(false)}
      />
      <FormContainer title="swap">
        <Input
          error={isNaN(fromValues.amount)}
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
          error={isNaN(toValues.amount)}
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
              <span>{`${pact.slippage*100}%`}</span>
            </RowContainer>
            <RowContainer style={{ marginTop: 5 }}>
              <Label>liquidity provider fee</Label>
              <span>{`${reduceBalance(pact.liquidityProviderFee * parseFloat(fromValues.amount))} ${fromValues.coin}`}</span>
            </RowContainer>
          </>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={getButtonLabel() !== "SWAP" || isNaN(fromValues.amount) || isNaN(toValues.amount)}
          loading={loading}
          onClick={async () => {
            setLoading(true)
            if (pact.signing.method !== 'sign') {
              const res = await pact.swapLocal(
                  { amount: fromValues.amount, address: fromValues.address },
                  { amount: toValues.amount, address: toValues.address },
                  (fromNote === "(estimated)" ? false : true)
                )
              if (res === -1) {
                setLoading(false)
                alert('Incorrect password. If forgotten, you can reset it with your private key')
                return
              } else {
                setShowTxModal(true)
                if (res?.result?.status === 'success') {
                  setFromValues({ amount: '', balance: '', coin: '', address: '' });
                  setToValues({ amount: '', balance: '', coin: '', address: '' })
                }
                setLoading(false)
              }
            } else {
              pact.swapWallet(
                { amount: fromValues.amount, address: fromValues.address },
                { amount: toValues.amount, address: toValues.address },
                (fromNote === "(estimated)" ? false : true)
              )
              // setLoading(false)
            }

          }}
        >
          {getButtonLabel()}
        </Button>
      </FormContainer>
    </Container>
  );
};

export default SwapContainer;
