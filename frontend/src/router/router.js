import React from 'react';
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';
import Layout from '../components/layout/Layout';
import PoolContainer from '../containers/PoolContainer';
import SwapContainer from '../containers/SwapContainer';
import WrapContainer from '../containers/WrapContainer';
import StatsContainer from '../containers/StatsContainer';
import StaticContainer from '../containers/StaticContainer';
import { ROUTE_INDEX, ROUTE_POOL, ROUTE_SWAP, ROUTE_WRAP, ROUTE_STATS, ROUTE_STATIC } from './routes';

export default () => {
  return (
    <Router>
      <Layout>
        <Switch>
          <Route exact path={ROUTE_INDEX} component={SwapContainer} />
          <Route exact path={ROUTE_SWAP} component={SwapContainer} />
          <Route exact path={ROUTE_POOL} component={PoolContainer} />
          <Route exact path={ROUTE_WRAP} component={WrapContainer} />
          <Route exact path={ROUTE_STATS} component={StatsContainer} />
          <Route exact path={ROUTE_STATIC} component={StaticContainer} />
        </Switch>
      </Layout>
    </Router>
  );
};
