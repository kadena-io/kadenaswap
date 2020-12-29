import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import Input from '../../components/shared/Input';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import Button from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import TokenSelector from '../../components/shared/TokenSelector';
import { throttle, debounce } from "throttle-debounce";
import { PactContext } from '../../contexts/PactContext'
import { ReactComponent as LeftIcon } from '../../assets/images/shared/left-arrow.svg';
import reduceBalance from '../../utils/reduceBalance';
import TxView from '../../components/shared/TxView';
import ReviewTx from './ReviewTx';

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

const ColumnContainer = styled.div`
  display: flex;
  flex-flow: column;
  align-items: center;

  & > span:first-child {
    font-size: 14px;
    margin-bottom: 3px;
  }

  & > span:last-child {
    font-size: 12px;
    color: #b5b5b5;
  }
`;

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
  margin: 15px 0;
`;

const LiquidityContainer = (props) => {
  const pact = useContext(PactContext);
  const {selectedView, setSelectedView} = props;
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [inputSide, setInputSide] = useState("")
  const [fromValues, setFromValues] = useState({coin: null, account: null, guard: null, balance: null,  amount: '' });
  const [toValues, setToValues] = useState({coin: null, account: null, guard: null, balance: null,  amount: '' });
  const [pairExist, setPairExist] = useState(false)
  const [showTxModal, setShowTxModal] = useState(false)
  const [showReview, setShowReview] = React.useState(false)
  const [loading, setLoading] = useState(false)

  useEffect(async () => {
    if (tokenSelectorType === 'from') setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') setSelectedToken(toValues.coin);
    else setSelectedToken(null);
  }, [tokenSelectorType]);

  useEffect(async () => {
    if (fromValues.coin){
      await pact.getTokenAccount(cryptoCurrencies[fromValues.coin].name, pact.account.account, true);
    }
    if (toValues.coin){
      await pact.getTokenAccount(cryptoCurrencies[toValues.coin].name, pact.account.account, false);
    }
    if (fromValues.coin && toValues.coin) {
      await pact.getPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name);
      await pact.getReserves(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name);
      if (pact.pair) {
        setPairExist(true)
      }
    }
  }, [fromValues, toValues,pairExist, pact.account.account]);

  const onTokenClick = ({ crypto }) => {
    if (tokenSelectorType === 'from') {
      setFromValues((prev) => ({ ...prev, coin: crypto.code, amount:0 }))
      setToValues((prev) => ({ ...prev, amount:0 }))
    };
    if (tokenSelectorType === 'to') {
      setToValues((prev) => ({ ...prev, coin: crypto.code, amount:0 }));
      setFromValues((prev) => ({ ...prev, amount:0 }))
    }
  };

  useEffect(() => {
    if (inputSide === 'from' && fromValues.amount !== "") {
      setInputSide(null)
      if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio)) {
        if (fromValues.amount.length < 5) {
          throttle(500, setToValues({ ...toValues, amount: parseFloat(fromValues.amount / pact.ratio).toPrecision(13) }))
        } else {
          debounce(500, setToValues({ ...toValues, amount: parseFloat(fromValues.amount / pact.ratio).toPrecision(13) }))
        }
      }
    }
    if (isNaN(pact.ratio) || fromValues.amount === "") {
      if (selectedView === "Add Liquidity"){
        setToValues((prev) => ({ ...prev, amount: '' }))
      }
    }
  }, [fromValues.amount])

  useEffect(() => {
    if (inputSide === 'to' && toValues.amount !== "") {
      setInputSide(null)
      if (fromValues.coin !== '' && toValues.coin !== '' && !isNaN(pact.ratio)) {
        if (toValues.amount.length < 5) {
          throttle(500, setFromValues({ ...fromValues, amount: parseFloat(toValues.amount * pact.ratio).toPrecision(13) }))
        } else {
          debounce(500, setFromValues({ ...fromValues, amount: parseFloat(toValues.amount * pact.ratio).toPrecision(13) }))
        }
      }
    }
    if (isNaN(pact.ratio) || toValues.amount === "") {
      if (selectedView === "Add Liquidity"){
        setFromValues((prev) => ({ ...prev, amount: '' }))
      }
    }
  }, [toValues.amount])

  useEffect(() => {
    if (pact.walletSuccess) {
      setLoading(false)
      setFromValues({coin: null, account: null, guard: null, balance: null,  amount: '' }});
      setToValues({coin: null, account: null, guard: null, balance: null, amount: '' }})
      pact.setWalletSuccess(false)
    }
  }, [pact.walletSuccess])

  const buttonStatus = () => {
    let status = {
      0: {msg: "Connect your KDA wallet", status: false},
      1: {msg: "Enter An Amount", status: false},
      2: {msg: "Supply", status: true},
      3: {msg: (token) => `Insufficient ${token} Balance`, status: false},
      4: {msg:"Create A Pair", status: true},
      5: {msg:"Pair Already Exists", status: false},
      6: {msg: "Select different tokens", status: false}
    }
    if (!pact.account.account) return status[0];
    if (selectedView==="Create A Pair") {
     if (pairExist) {
       setSelectedView("Add Liquidity")
     }
     if (fromValues.coin && toValues.coin && fromValues.amount && toValues.amount){
       return status[4];
     }
     else if (!fromValues.amount || !toValues.amount) return status[1];
     else if (fromValues.amount > pact.tokenFromAccount.balance) return {...status[3], msg: status[3].msg(fromValues.coin)};
     else if (toValues.amount > pact.tokenToAccount.balance) return {...status[3], msg: status[3].msg(toValues.coin)};
     else if (fromValues.coin === toValues.coin) return status[6];
     else return status[4]
   }
   else if (!fromValues.amount || !toValues.amount) return status[1];
   else if (fromValues.amount > pact.tokenFromAccount.balance) return {...status[3], msg: status[3].msg(fromValues.coin)};
   else if (toValues.amount > pact.tokenToAccount.balance) return {...status[3], msg: status[3].msg(toValues.coin)};
   else if (fromValues.coin === toValues.coin) return status[6];
   else {
     if (isNaN(pact.ratio)) {
       return {...status[2], status: false};
     } else
     return status[2];
   }
  }

  const supply = async () => {
      if (selectedView==="Create A Pair"){
        setLoading(true)
        await pact.createTokenPairLocal(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount)
        setLoading(false)
        setShowReview(false)
        setShowTxModal(true)
      } else {
        if (pact.signing.method !== 'sign') {
          const res = await pact.addLiquidityLocal(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
          console.log('res', res)
          console.log(typeof res)
          if (res === -1) {
            setLoading(false)
            alert('Incorrect password. If forgotten, you can reset it with your private key')
            return
          } else {
            setShowTxModal(true)
            if (res?.result?.status === 'success') {
              setFromValues({coin: null, account: null, guard: null, balance: null, amount: '' }});
              setToValues({coin: null, account: null, guard: null, balance: null, amount: ''})
            }
            setLoading(false)
            setShowReview(false)
          }
        } else {
          pact.addLiquidityWallet(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
          setFromValues({coin: null, account: null, guard: null, balance: null, amount: ''});
          setToValues({coin: null, account: null, guard: null, balance: null, amount: ''})
          setShowReview(false)
        }
      }
  }

  return (
      <FormContainer title={selectedView}>
        <TokenSelector
          show={tokenSelectorType !== null}
          selectedToken={selectedToken}
          onTokenClick={onTokenClick}
          onClose={() => setTokenSelectorType(null)}
        />
        <LeftIcon style={{ cursor: 'pointer', position: 'absolute', width:20, height: 30, top: 14, left: 14 }} onClick={() => props.closeLiquidity()} />
        <Input
          leftLabel="input"
          rightLabel={`balance: ${reduceBalance(pact.tokenFromAccount.balance) ?? '-'}`}
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
        <ButtonDivider icon={<PlusIcon />} buttonStyle={{ cursor: 'default' }} />
        <Input
          leftLabel="input"
          rightLabel={`balance: ${reduceBalance(pact.tokenToAccount.balance) ?? '-'}`}
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
        {fromValues.coin && toValues.coin && (
          <>
            <Label>price and pool share</Label>
            <RowContainer style={{ marginBottom: 0 }}>
              <ColumnContainer>
                <span>{reduceBalance(pact.getRatio(toValues.coin, fromValues.coin))}</span>
                <span>{`${toValues.coin} per ${fromValues.coin}`}</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>{reduceBalance(pact.getRatio1(toValues.coin, fromValues.coin))}</span>
                <span>{`${fromValues.coin} per ${toValues.coin}`}</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>{!pact.share(fromValues.amount) ? 0 : reduceBalance(pact.share(fromValues.amount)*100)}%</span>
                <span>Share of Pool</span>
              </ColumnContainer>
            </RowContainer>
          </>
        )}
        <TxView
          view={selectedView}
          show={showTxModal}
          token0={fromValues.coin}
          token1={toValues.coin}
          createTokenPair={() => pact.createTokenPairLocal(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount)}
          onClose={() => setShowTxModal(false)}
        />
        <ReviewTx
          fromValues={fromValues}
          toValues={toValues}
          buttonStatus = {buttonStatus}
          liquidityView = {selectedView}
          supply={supply}
          loading={loading}
          open={showReview}
          setOpen={setShowReview}
        />
    </FormContainer>
  );
};

export default LiquidityContainer;
