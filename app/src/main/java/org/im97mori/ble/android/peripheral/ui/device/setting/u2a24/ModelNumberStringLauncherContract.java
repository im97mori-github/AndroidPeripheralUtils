package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ModelNumberStringLauncherContract extends ActivityResultContract<String, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable String input) {
        Intent intent = new Intent(context.getApplicationContext(), ModelNumberStringSettingActivity.class);
        intent.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
