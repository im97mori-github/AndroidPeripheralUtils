package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class IntermediateCuffPressureLauncherContract extends ActivityResultContract<byte[], byte[]> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable byte[] input) {
        Intent intent = new Intent(context.getApplicationContext(), IntermediateCuffPressureSettingActivity.class);
        intent.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), input);
        return intent;
    }

    @Override
    public byte[] parseResult(int resultCode, @Nullable Intent intent) {
        byte[] data;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            data = intent.getByteArrayExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString());
        } else {
            data = null;
        }
        return data;
    }
}
