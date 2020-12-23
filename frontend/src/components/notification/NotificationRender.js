import React from 'react';
import { NotificationConsumer, NotificationProvider } from '../../contexts/NotificationContext';
import Notification from './Notification';

const NotificationRender = ({ children }) => {
  return (
    <NotificationProvider>
      {children}
      <NotificationConsumer>{(notificationValues) => <Notification notificationValues={notificationValues} />}</NotificationConsumer>
    </NotificationProvider>
  );
};

export default NotificationRender;
