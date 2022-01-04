package org.im97mori.ble.android.peripheral.utils;


import android.text.Editable;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.im97mori.stacklog.LogUtils;

public final class Utils {

    private Utils() {
    }

//    private static class CharacteristicDataDeserializer implements JsonDeserializer<CharacteristicData> {
//        private static final Type DESCRIPTOR_LIST_TYPE = new TypeToken<ArrayList<DescriptorData>>() {
//        }.getType();
//
//        @Override
//        public CharacteristicData deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
//            JsonObject jsonObject = json.getAsJsonObject();
//            UUID uuid = UUID.fromString(jsonObject.get("uuid").getAsString());
//            CharacteristicData data;
//            if (CharacteristicUUID.EMAIL_ADDRESS_CHARACTERISTIC.equals(uuid)) {
//                data = context.deserialize(json, CharacteristicData2.class);
//            } else {
//            data = new CharacteristicData(uuid
//                    , context.deserialize(jsonObject.get("property"), int.class)
//                    , context.deserialize(jsonObject.get("permission"), int.class)
//                    , context.deserialize(jsonObject.get("descriptor_data_list"), DESCRIPTOR_LIST_TYPE)
//                    , context.deserialize(jsonObject.get("response_code"), int.class)
//                    , context.deserialize(jsonObject.get("delay"), long.class)
//                    , context.deserialize(jsonObject.get("data"), byte[].class)
//                    , context.deserialize(jsonObject.get("notification_count"), int.class));
//            }
//            return data;
//        }
//
//    }

    private static Gson mGson;

    @NonNull
    public synchronized static Gson createGsonInstance() {
        if (mGson == null) {
            mGson = new GsonBuilder()
//                    .registerTypeAdapter(CharacteristicData.class, new CharacteristicDataDeserializer())
                    .create();
        }
        return mGson;
    }

    public static void setTextDistinct(@NonNull EditText editText, @Nullable CharSequence text) {
        Editable currentText = editText.getText();
        if ((currentText == null && text != null)
                || (currentText != null && text == null)
                || (currentText != null && !currentText.toString().equals(text.toString()))) {
            if (editText instanceof AutoCompleteTextView) {
                ((AutoCompleteTextView) editText).setText(text, false);
            } else {
                editText.setText(text);
            }
        }
    }

    public static void stackLog(Object... args) {
        LogUtils.stackLogWithOffset(2, args);
    }

}
