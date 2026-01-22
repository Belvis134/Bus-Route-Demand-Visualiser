document.addEventListener('DOMContentLoaded', function() {
  // Fetch Datamall data via fly.io
  Shiny.addCustomMessageHandler('fetch_datamall', function(params) {
    document.getElementById('upload_conf').innerHTML =
      '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Importing from Datamall, please wait...</span>';
    var datamall_date_raw = document.getElementById("datamall_date").value;
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var datamall_date = datamall_date_raw.replace("-", "");
    const data_type = document.getElementById("datamall_data_type").value;
    const data_type2 = document.getElementById("datamall_data_type2").value;
    if (!datamall_date_raw) {
      throw new Error("Date not defined. If you are requesting BusRouter data from repository, you need a date!")
    }
    const encoded_account_key = encodeURIComponent(params.account_key);
    const csv_proxy_url = 'https://stcraft.myddns.me/datamall-proxy' +
      '?date=' + datamall_date +
      '&account_key=' + encoded_account_key +
      '&data_type=' + data_type +
      '&data_type2=' + data_type2;
    fetch(csv_proxy_url)
      .then(response => {
        if (!response.ok) {
          return response.json().then(text => {
            throw new Error(
              `${text.error}`
            );
          });
        }
        return response.text();
      })
      .then(function(csv_text) {
        // Pass the CSV text to Shiny.
        Shiny.setInputValue('csv_data_in', { data1: csv_text });
        if (data_type == 'origin_destination') {
          if (data_type2 == 'bus') {
            var msg = "O-D Bus"
          } else {
            var msg = "O-D Train"
          }
        } else if (data_type == 'specific_stop') {
          if (data_type2 == 'bus') {
            var msg = "Specific Bus Stop"
          } else {
            var msg = "Specific MRT/LRT Station"
          }
        }
        document.getElementById('upload_conf').innerHTML =
          `<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> ${msg} data import from Datamall successful!</span>`;
      })
      .catch(err => {
        console.error(err);
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' + err.message + '</span>';
      });
  });

  // Fetch JSON data (data2 & data3) from BusRouter.
  Shiny.addCustomMessageHandler('fetch_busrouter', function(params) {
    document.getElementById('upload_conf2').innerHTML =
    '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Importing from BusRouter, please wait...</span>';
    var json_urls = [
      'https://data.busrouter.sg/v1/services.json',
      'https://data.busrouter.sg/v1/stops.json'
    ];
    return Promise.all(
      json_urls.map(function(url) {
        return fetch(url).then(function(response) {
          if (!response.ok) {
            throw new Error(`BusRouter gave ${response.status} because of ${response.statusText}`);
          }
          return response.json();
        });
      })
    ).then(function(return_json_data) {
      var json_data = {
        data2: return_json_data[0],
        data3: return_json_data[1]
      };
      Shiny.setInputValue('json_data_in', JSON.stringify(json_data));
      document.getElementById('upload_conf2').innerHTML =
        '<span style=\"color:#00DD00; font-weight:bold;\"><i class=\"fas fa-square-check\"></i> File import from BusRouter successful!</span>';
    })
    .catch(err => {
      console.error(err);
      document.getElementById('upload_conf2').innerHTML =
          '<span style=\"color:#BB0000; font-weight:bold;\"><i class=\"fas fa-triangle-exclamation\"></i> ' + err + '</span>';
    });
  });

  // Fetch data from Google Drive repo.
  Shiny.addCustomMessageHandler('fetch_drive_datamall', function(params) {
    document.getElementById('upload_conf').innerHTML =
      '<span style="color:#2050C0; font-weight:bold;"><i class="fas fa-hourglass-half"></i> Importing Datamall data from repository, please wait...</span>';
    // Get the selected dates from the inputs.
    var datamall_date_raw = document.getElementById("od_matrix_date").value;
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var datamall_date = datamall_date_raw.replace("-", "");
    const data_type = document.getElementById("datamall_data_type").value;
    const data_type2 = document.getElementById("datamall_data_type2").value;
    if (!datamall_date_raw) {
      throw new Error("Date not defined. If you are requesting BusRouter data from repository, you need a date!")
    }
    
    // Fetch Datamall CSV data from repository via its dedicated endpoint:
    var datamall_repository = `https://stcraft.myddns.me/repository/datamall?datamall_date=${datamall_date}&data_type=${data_type}&data_type2=${data_type2}`;
    fetch(datamall_repository)
      .then(response => {
        if (!response.ok) {
          // If the response status indicates an error, parse the error
          return response.text().then(text => {
            throw new Error(text);
          });
        }
        return response.text();
      })
      .then(function(csv_data) {
        if (data_type == 'origin_destination') {
          if (data_type2 == 'bus') {
            var msg = "O-D Bus"
          } else {
            var msg = "O-D Train"
          }
        } else if (data_type == 'specific_stop') {
          if (data_type2 == 'bus') {
            var msg = "Specific Bus Stop"
          } else {
            var msg = "Specific MRT/LRT Station"
          }
        }
        Shiny.setInputValue("csv_data_in", { data1: csv_data });
        document.getElementById('upload_conf').innerHTML =
          `<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> Datamall ${msg} data import from repository successful!</span>`;
      })
      .catch(error => {
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' +
          error.message + '</span>';
      });
  });

   Shiny.addCustomMessageHandler('fetch_drive_busrouter', function(params) {
    document.getElementById('upload_conf2').innerHTML =
      '<span style="color:#2050C0; font-weight:bold;"><i class="fas fa-hourglass-half"></i> Importing BusRouter data from repository, please wait...</span>';
    // Get the selected dates from the inputs.
    var busrouter_date_raw = document.getElementById("busrouter_date").value;
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var busrouter_date = busrouter_date_raw.replace("-", "");
    const data_type = document.querySelector('input[name="json_data_type"]:checked').value;
    if (data_type === "bus" && !busrouter_date_raw) {
      throw new Error("Date not defined. If you are requesting BusRouter data from repository, you need a date!")
    }
    // Fetch BusRouter or MRT line and station names JSON data from repository via a dedicated endpoint:
    const endpoint = `https://stcraft.myddns.me/repository/busrouter?busrouter_date=${busrouter_date}&data_type=${data_type}`
    fetch(endpoint)
      .then(response => {
        if (!response.ok) {
          return response.text().then(text => {
            throw new Error(text);
          });
        }
        return response.json();
      })
      .then(function(busrouter_data) {
        Shiny.setInputValue('json_data_in', busrouter_data);
        document.getElementById('upload_conf2').innerHTML =
          `<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> ${(data_type === "bus") ? "BusRouter" : "MRT lines and station names"} data import from repository successful!</span>`;
      })
      .catch(error => {
        document.getElementById('upload_conf2').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' +
          error.message + '</span>';
      });
  });

  Shiny.addCustomMessageHandler('fetch_routes', function(params) {
    document.getElementById('result_conf2').innerHTML =
      '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Importing from Datamall, please wait...</span>';
    // var datamall_date_raw = document.getElementById("datamall_date").value;
    // // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    // var datamall_date = datamall_date_raw.replace("-", "");
    const data_type = 'routes' // document.getElementById("datamall_data_type").value;
    const data_type2 = 'bus' // document.getElementById("datamall_data_type2").value;
    // if (!datamall_date_raw) {
    //   throw new Error("Date not defined. If you are requesting BusRouter data from repository, you need a date!")
    // }
    const encoded_account_key = encodeURIComponent(params.account_key);
    const csv_proxy_url = 'https://stcraft.myddns.me/datamall-proxy' +
      '?date=' + datamall_date +
      '&account_key=' + encoded_account_key +
      '&data_type=' + data_type +
      '&data_type2=' + data_type2;
    fetch(csv_proxy_url)
      .then(response => {
        if (!response.ok) {
          return response.json().then(text => {
            throw new Error(
              `${text.error}`
            );
          });
        }
        return response.json();
      })
      .then(function(routes_json) {
        // Pass the JSON to Shiny.
        Shiny.setInputValue('routes_data_in', JSON.stringify(routes_json))
        document.getElementById('result_conf2').innerHTML =
          `<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> Routes data import from Datamall successful!</span>`;
      })
      .catch(err => {
        console.error(err);
        document.getElementById('result_conf2').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' + err.message + '</span>';
      });
  })

  // Clear cache upon refresh
  window.onbeforeunload = function() {
    // Clear session storage
    sessionStorage.clear();
    // Clear local storage (or specific cache keys)
    localStorage.clear();
    // Clear caches created via the Cache API
    if ('caches' in window) {
      caches.keys().then(function(names) {
        names.forEach(function(name) {
          caches.delete(name);
        });
      });
    }
  };

  // Auto-adjust iframe height.
  function send_height() {
    // Compute the document's height
    var height = document.documentElement.scrollHeight || document.body.scrollHeight;
    // Send the height to the parent window
    window.parent.postMessage({ iframe_height: height }, "*");
  }
  
  // Call it when loaded and possibly on resize/mutation if dynamic
  window.addEventListener("load", send_height);
  // Optionally update on resize or changes:
  window.addEventListener("resize", send_height);
})