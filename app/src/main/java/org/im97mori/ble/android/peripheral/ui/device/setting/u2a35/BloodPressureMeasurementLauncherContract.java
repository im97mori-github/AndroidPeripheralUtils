package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class BloodPressureMeasurementLauncherContract extends ActivityResultContract<String, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable String input) {
        Intent intent = new Intent(context.getApplicationContext(), BloodPressureMeasurementSettingActivity.class);
        intent.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
