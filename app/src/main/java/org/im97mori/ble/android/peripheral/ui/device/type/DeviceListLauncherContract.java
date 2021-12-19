package org.im97mori.ble.android.peripheral.ui.device.type;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.Constants;

public class DeviceListLauncherContract extends ActivityResultContract<Void, Integer> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, Void input) {
        return new Intent(context.getApplicationContext(), DeviceTypeListActivity.class);
    }

    @Override
    public Integer parseResult(int resultCode, @Nullable Intent intent) {
        Integer type;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            type = intent.getIntExtra(DeviceTypeListActivity.KEY_DEVICE_TYPE, Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        } else {
            type = null;
        }
        return type;
    }
}
