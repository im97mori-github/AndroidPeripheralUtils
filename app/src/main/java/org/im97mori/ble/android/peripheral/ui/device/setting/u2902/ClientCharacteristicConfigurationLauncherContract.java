package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

public class ClientCharacteristicConfigurationLauncherContract extends ActivityResultContract<Pair<String, Integer>, String> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @NonNull Pair<String, Integer> input) {
        Intent intent = new Intent(context.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class);
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), input.first);
        intent.putExtra(ClientCharacteristicConfigurationSettingViewModel.KEY_PROPERTIES_TYPE, input.second);
        return intent;
    }

    @Override
    public String parseResult(int resultCode, @Nullable Intent intent) {
        String dataString;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            dataString = intent.getStringExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString());
        } else {
            dataString = null;
        }
        return dataString;
    }
}
