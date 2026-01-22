// ---Receiving Discord data---

let last_hash = '';

function compute_hash(data) {
  const encoder = new TextEncoder();
  const data_string = JSON.stringify(data);
  const data_buffer = encoder.encode(data_string);
  return crypto.subtle.digest('SHA-256', data_buffer).then(hash_buffer => {
    // Convert hash buffer to hex string
    const hash_array = Array.from(new Uint8Array(hash_buffer));
    const hash_hex = hash_array.map(b => b.toString(16).padStart(2, '0')).join('');
    return hash_hex;
  });
}

function pollForData() {
  const storage_url = "https://stcraft.myddns.me:3000/discord-data-in";

  fetch(storage_url)
    .then(response => {
      if (!response.ok) {
        throw new Error("Failed to fetch incoming Discord data from fly.io.");
      }
      return response.json();
    })
    .then(inbound_discord_data => {
      if (inbound_discord_data) {
        compute_hash(inbound_discord_data).then(current_hash => {
          // Only log and process if the new data hash is different than the last one.
          if (current_hash !== last_hash) {
            console.log("New CSV data received:", inbound_discord_data);
            last_hash = current_hash; // Update our stored hash.
            // Send data to BRDV (Shiny app).
            Shiny.setInputValue("discord_data", inbound_discord_data);
          }
        });
      }
    })
    .catch(error => {
      console.error("Error fetching data from fly.io:", error);
    });
}

// ---Sending image data---

Shiny.addCustomMessageHandler("send_image", function(image_data) {
  // Get the user ID from the inbound Discord data defined above.
  let user_id = last_data.user_id;
  // Post to the /heatmap endpoint.
  fetch("https://stcraft.myddns.me:3000/data/heatmap", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      user_id: user_id,
      image: image_data.src,
      width: image_data.width,
      height: image_data.height,
      alt: image_data.alt
    })
  })
  .then(response => response.json())
  .then(data => {
    if(data.status === "success"){
      console.log("Heatmap image uploaded. Returned URL:", data.url);
    } else {
      console.error("Error from /heatmap server:", data.message);
    }
  })
  .catch(error => {
    console.error("Error uploading heatmap image:", error);
  });
});