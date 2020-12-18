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
import { PactContext } from '../../contexts/PactContext'
import { ReactComponent as LeftIcon } from '../../assets/images/shared/left-arrow.svg';
import reduceBalance from '../../utils/reduceBalance';
import TxView from '../../components/shared/TxView';

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
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const liquidityView = props.selectedView;
  const [fromValues, setFromValues] = useState({ ...pact.account, coin: cryptoCurrencies.KDA.code });
  const [toValues, setToValues] = useState({ ...pact.tokenAccount, coin:cryptoCurrencies.ABC.code });
  const [showTxModal, setShowTxModal] = useState(false)
  const [loading, setLoading] = useState(false)

  useEffect(async () => {
    await pact.getTokenAccount(cryptoCurrencies[fromValues.coin].name, pact.account.account, true);
    await pact.getTokenAccount(cryptoCurrencies[toValues.coin].name, pact.account.account, false);
    await pact.getReserves(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name);
    await pact.getPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name);
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType, fromValues, toValues]);

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

  const setTokenAmount = (amount1, amount2) => {
    if (amount1) {
      setFromValues((prev) => ({ ...prev, amount: reduceBalance(amount1) }));
      setToValues((prev) => ({ ...prev, amount: reduceBalance(amount1 * pact.getRatio(toValues.coin, fromValues.coin)) }));
    } else if (amount2){
      setToValues((prev) => ({ ...prev, amount: reduceBalance(amount2)}));
      setFromValues((prev) => ({ ...prev, amount: reduceBalance(amount2 * pact.getRatio1(toValues.coin, fromValues.coin)) }));
    }
  }

  const buttonStatus = () => {
    let status = {
      0: {msg: "Connect Wallet", status: false},
      1: {msg: "Enter An Amount", status: true},
      2: {msg: "Supply", status: true},
      3: {msg: (token) => `Insufficient ${token} Balance`, status: false},
      4: {msg:"Pair Already Exists", status: false},
      5: {msg: "Select different tokens", status: false}
    }
    if (!fromValues.amount && !toValues.amount) return status[1];
    else if (props.liquidityView==="Create A Pair" && pact.pair) return status[4];
    else if (!pact.account.account || (fromValues.amount > pact.tokenFromAccount.balance)) return {...status[3], msg: status[3].msg(fromValues.coin)};
    else if (toValues.amount > pact.tokenToAccount.balance) return {...status[3], msg: status[3].msg(toValues.coin)};
    else if (fromValues.coin === toValues.coin) return status[5];
    else return status[2];
  }

  const supply = async () => {
      if (props.liquidityView==="Create A Pair"){
        // setLoading(true)
        await pact.createTokenPair(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount).then(console.log)
        // setLoading(false)
        // setShowTxModal(true)
      } else{
        setLoading(true)
        await pact.addLiquidityLocal(cryptoCurrencies[fromValues.coin].name, cryptoCurrencies[toValues.coin].name, fromValues.amount, toValues.amount);
        setLoading(false)
        setShowTxModal(true)
      }
  }

  return (
      <FormContainer title={liquidityView}>
        <TxView
          view="addLiquidity"
          show={showTxModal}
          token0={fromValues.coin}
          token1={toValues.coin}
          onClose={() => setShowTxModal(false)}
        />
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
          onChange={(e, { value }) => {
            setTokenAmount(value);
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
          onChange={(e, { value }) => setTokenAmount(null, value)}
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
                <span>{reduceBalance(pact.share(fromValues.amount)*100)}%</span>
                <span>Share of Pool</span>
              </ColumnContainer>
            </RowContainer>
          </>
        )}
        <Button
          disabled={!buttonStatus().status}
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          loading={loading}
          onClick={supply}>
          {buttonStatus().msg}
        </Button>
      </FormContainer>
  );
};

export default LiquidityContainer;
