import React, { createContext } from 'react';
import { toast } from 'react-toastify';
import NotificationContainer from '../components/notification/NotificationContainer';

export const NotificationContext = createContext();

export const STATUSES = {
  DEFAULT: toast.TYPE.DEFAULT,
  SUCCESS: toast.TYPE.SUCCESS,
  WARNING: toast.TYPE.WARNING,
  DARK: toast.TYPE.DARK,
  ERROR: toast.TYPE.ERROR,
  INFO: toast.TYPE.INFO
};

export const NotificationProvider = ({ children }) => {
  const showNotification = ({
    title = '',
    message = '',
    autoClose = false,
    position = 'top-right',
    type = STATUSES.SUCCESS,
    style = undefined,
    progressStyle = undefined,
    hideProgressBar = true,
    pauseOnHover = undefined,
    pauseOnFocusLoss = undefined,
    draggable = undefined,
    delay = undefined,
    closeButton = undefined,
    onOpen = undefined,
    onClose = undefined
  }) => {
    return toast(<NotificationContainer message={message} type={type} title={title} />, {
      title,
      message,
      autoClose,
      position,
      style,
      type,
      progressStyle,
      hideProgressBar,
      pauseOnHover,
      pauseOnFocusLoss,
      draggable,
      delay,
      closeButton,
      onOpen,
      onClose
    });
  };

  return (
    <NotificationContext.Provider
      value={{
        showNotification
      }}
    >
      {children}
    </NotificationContext.Provider>
  );
};

export const NotificationConsumer = NotificationContext.Consumer;
