package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ModelNumberStringLauncherContract extends ActivityResultContract<byte[], byte[]> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable byte[] input) {
        Intent intent = new Intent(context.getApplicationContext(), ModelNumberStringSettingActivity.class);
        intent.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public byte[] parseResult(int resultCode, @Nullable Intent intent) {
        byte[] data;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            data = intent.getByteArrayExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString());
        } else {
            data = null;
        }
        return data;
    }
}
