package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class DeviceInformationServiceLauncherContract extends ActivityResultContract<String, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable String input) {
        Intent intent = new Intent(context.getApplicationContext(), DeviceInformationServiceSettingActivity.class);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), input);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
