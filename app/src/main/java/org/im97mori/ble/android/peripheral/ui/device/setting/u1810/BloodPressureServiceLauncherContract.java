package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class BloodPressureServiceLauncherContract extends ActivityResultContract<String, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable String input) {
        Intent intent = new Intent(context.getApplicationContext(), BloodPressureServiceSettingActivity.class);
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), input);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
