import swal from '@sweetalert/with-react'

export default async () => {
  return await swal({
    text: "Follow instructions in the wallet to preview and sign your transaction",
    title: "Please Sign",
    showSpinner: true,
  })
};
