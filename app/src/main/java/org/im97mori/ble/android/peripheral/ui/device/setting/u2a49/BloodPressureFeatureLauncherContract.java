package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class BloodPressureFeatureLauncherContract extends ActivityResultContract<byte[], byte[]> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable byte[] input) {
        Intent intent = new Intent(context.getApplicationContext(), BloodPressureFeatureSettingActivity.class);
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public byte[] parseResult(int resultCode, @Nullable Intent intent) {
        byte[] data;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            data = intent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString());
        } else {
            data = null;
        }
        return data;
    }
}
