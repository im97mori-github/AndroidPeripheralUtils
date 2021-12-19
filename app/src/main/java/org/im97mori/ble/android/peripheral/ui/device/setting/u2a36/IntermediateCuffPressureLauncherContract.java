package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class IntermediateCuffPressureLauncherContract extends ActivityResultContract<String, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable String input) {
        Intent intent = new Intent(context.getApplicationContext(), IntermediateCuffPressureSettingActivity.class);
        intent.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
