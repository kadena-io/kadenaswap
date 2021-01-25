const fetch = require("node-fetch");
const cmd = {
  asset: 'kadena'
}
var mkReq = function(cmd) {
  return {
    headers: {
      "Content-Type": "application/json"
    },
    method: "POST",
    body: JSON.stringify(cmd)
  };
};
var parseRes = async function (raw) {
  const rawRes = await raw;
  const res = await rawRes;
  if (res.ok){
     const resJSON = await rawRes.json();
     return resJSON;
   } else {
     const resTEXT = await rawRes.text();
     return resTEXT;
   }
};
const getAccounts = async () => {
  try {
    let res = await fetch('http://127.0.0.1:9467/v1/accounts', mkReq(cmd))
    let pRes = await parseRes(res)
    return pRes
  } catch (e) {
    return -1
  }
}

export default getAccounts;
