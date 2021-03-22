import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as SwapArrowsIcon } from '../assets/images/shared/swap-arrows.svg';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import {reduceBalance, limitDecimalPlaces} from '../utils/reduceBalance';
import TokenSelector from '../components/shared/TokenSelector';
import TxView from '../components/shared/TxView';
import { PactContext } from '../contexts/PactContext';
import { throttle, debounce } from "throttle-debounce";
import pwError from '../components/alerts/pwError'

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
  const [fromValues, setFromValues] = useState({ amount: '', balance: '', coin: '', address: '', precision: 0 });
  const [toValues, setToValues] = useState({ amount: '', balance: '', coin: '', address: '', precision: 0 });
  const [inputSide, setInputSide] = useState("")
  const [fromNote, setFromNote] = useState("")
  const [toNote, setToNote] = useState("")
  const [showTxModal, setShowTxModal] = useState(false)
  const [loading, setLoading] = useState(false)
  const [fetchingPair, setFetchingPair] = useState(false)
  const [priceImpact, setPriceImpact] = useState("")
  const pact = useContext(PactContext);

  useEffect(() => {
    if (!isNaN(fromValues.amount)) {
      if (inputSide === 'from' && fromValues.amount !== "") {
        setToNote("(estimated)")
        setFromNote("")
        setInputSide(null)
        if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio)) {
          if (fromValues.amount.length < 5) {
            throttle(500, setToValues({ ...toValues, amount: reduceBalance(pact.computeOut(fromValues.amount), toValues.precision) }))
          } else {
            debounce(500, setToValues({ ...toValues, amount: reduceBalance(pact.computeOut(fromValues.amount), toValues.precision) }))
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
            console.log(pact.computeIn(toValues.amount))
            throttle(500, setFromValues({ ...fromValues, amount: reduceBalance(pact.computeIn(toValues.amount), fromValues.precision) }))
          } else {
            debounce(500, setFromValues({ ...fromValues, amount: reduceBalance(pact.computeIn(toValues.amount), fromValues.precision) }))
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
        setToValues({ ...toValues, amount: reduceBalance(pact.computeOut(fromValues.amount), toValues.precision) })
      } if (fromValues.amount === "" && toValues.amount !== "") {
        setFromValues({ ...fromValues, amount: reduceBalance(pact.computeIn(toValues.amount), fromValues.precision) })
      } if (fromValues.amount !== "" && toValues.amount !== "")  {
        setToValues({ ...toValues, amount: reduceBalance(pact.computeOut(fromValues.amount), toValues.precision) })
      }
    }
  }, [pact.ratio])

  useEffect(() => {
    if (!isNaN(pact.ratio)) {
      setPriceImpact(pact.computePriceImpact(Number(fromValues.amount), Number(toValues.amount)))
    } else {
      setPriceImpact("")
    }
  }, [fromValues.coin, toValues.coin, fromValues.amount, toValues.amount, pact.ratio])

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
    if (crypto.code === 'coin') {
      if (pact.account){
        balance = pact.account.balance
      }
    } else {
      let acct = await pact.getTokenAccount(crypto.code, pact.account.account, tokenSelectorType === 'from')
      if (acct){
        balance = pact.getCorrectBalance(acct.balance)
      }
    }
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: balance, coin: crypto.name, address: crypto.code, precision: crypto.precision }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: balance, coin: crypto.name, address: crypto.code, precision: crypto.precision }));
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
      <FormContainer title="swap">
        <Input
          error={isNaN(fromValues.amount)}
          leftLabel={`from ${fromNote}`}
          rightLabel={`balance: ${reduceBalance(fromValues.balance) ?? '-'}`}
          placeholder="enter amount"
          inputRightComponent={
            fromValues.coin ? (
              <InputToken
                icon={pact.tokenData[fromValues.coin].icon}
                code={pact.tokenData[fromValues.coin].name}
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
            setFromValues((prev) => ({ ...prev, amount: limitDecimalPlaces(value, fromValues.precision)}))
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
                icon={pact.tokenData[toValues.coin].icon}
                code={pact.tokenData[toValues.coin].name}
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
            setToValues((prev) => ({ ...prev, amount: limitDecimalPlaces(value, toValues.precision) }))
          }}
        />
        {(!isNaN(pact.ratio) && fromValues.amount && fromValues.coin && toValues.amount && toValues.coin
          ?
  	         <>
              <RowContainer>
                <Label>price</Label>
                <span>{`${reduceBalance(pact.ratio*(1+priceImpact))} ${fromValues.coin} per ${toValues.coin}`}</span>
              </RowContainer>
              <RowContainer style={{ marginTop: 5 }}>
                <Label>Price Impact</Label>
                <span style={{color: pact.priceImpactWithoutFee(priceImpact) > 0.1 ? "red" : "green" }}>{
                  pact.priceImpactWithoutFee(priceImpact)<0.0001 && pact.priceImpactWithoutFee(priceImpact)
                    ? "< 0.01%"
                    : `${reduceBalance(pact.priceImpactWithoutFee(priceImpact)*100, 4)}%`
                  }
                </span>
              </RowContainer>
              <RowContainer style={{ marginTop: 5 }}>
                <Label>max slippage</Label>
                <span>{`${pact.slippage*100}%`}</span>
              </RowContainer>
              <RowContainer style={{ marginTop: 5 }}>
                <Label>liquidity provider fee</Label>
                <span>{`${reduceBalance(pact.liquidityProviderFee * parseFloat(fromValues.amount),14)} ${fromValues.coin}`}</span>
              </RowContainer>
            </>
	        :
            <></>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={getButtonLabel() !== "SWAP" || isNaN(fromValues.amount) || isNaN(toValues.amount)}
          loading={loading}
          onClick={async () => {
            setLoading(true)
            if (pact.signing.method !== 'sign') {
              const res = await pact.swapLocal(
                  { amount: fromValues.amount, address: fromValues.address, coin: fromValues.coin },
                  { amount: toValues.amount, address: toValues.address, coin: toValues.coin },
                  (fromNote === "(estimated)" ? false : true)
                )
              if (res === -1) {
                setLoading(false)
                //error alert
                if (pact.localRes) pwError();
                return
              } else {
                setShowTxModal(true)
                if (res?.result?.status === 'success') {
                  setFromValues({ amount: '', balance: '', coin: '', address: '', precision: 0 });
                  setToValues({ amount: '', balance: '', coin: '', address: '', precision: 0 })
                }
                setLoading(false)
              }
            } else {
              const res = await pact.swapWallet(
                { amount: fromValues.amount, address: fromValues.address, coin: fromValues.coin },
                { amount: toValues.amount, address: toValues.address, coin: toValues.coin },
                (fromNote === "(estimated)" ? false : true)
              )
              setShowTxModal(true)
              if (res?.result?.status === 'success') {
                setFromValues({ amount: '', balance: '', coin: '', address: '', precision: 0 });
                setToValues({ amount: '', balance: '', coin: '', address: '', precision: 0 })
              }
              setLoading(false)
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
